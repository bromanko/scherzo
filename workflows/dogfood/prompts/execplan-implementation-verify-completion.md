Verify ExecPlan completion for Scherzo's `workflow:execplan-implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

Bundle preparation output:
{{ steps.prepare_bundle.stdout }}

Implementation step response:
{{ steps.implement_plan.final_response }}

Change analysis output:
{{ steps.analyze_changes.stdout }}

Verification contract:

- This is a plan-completion verification, not a code review. Do not critique style, formatting, architecture, or optional polish unless it blocks promised behavior.
- Do not edit tracked source, tests, workflows, docs, or the ExecPlan review doc. Your only allowed write is the verdict artifact at `tmp/scherzo-plan-completion-verdict.json`.
- Read `tmp/scherzo-implementation.json`, `tmp/execplan-bundle.json`, `tmp/execplan-review-doc.md`, and `tmp/execplan-implementation-pack.json`.
- Determine the checked-in review doc path from `tmp/scherzo-implementation.json` field `plan_path`, falling back to `review_doc.path` in `tmp/execplan-bundle.json`, and read that file. Treat the checked-in review doc as authoritative for current intent, scope, risks, milestones, progress, and acceptance. Treat `tmp/execplan-review-doc.md` as the prepared bundle baseline.
- Treat the implementation pack as the authoritative mechanical handoff only when it does not conflict with review-doc intent, scope, acceptance, or safety.
- Inspect the checked-in review doc's Progress, Validation and Acceptance, Milestones, Scope Boundaries, Open Questions, and any explicit non-goals/deferred/stretch sections.
- Compare the implementation summary and changed files/tests against the checked-in review doc and implementation pack. Inspect the smallest useful set of changed files and tests when the summary is not enough.
- Explicitly return `fail` when required Progress checklist items are still unchecked, required milestones/acceptance criteria are undelivered, or outcomes promised by the review doc are not observable.
- Do not fail for imperfect wording, formatting, or optional/stretch work that is clearly marked optional, stretch, deferred, or out of scope.
- Before writing the verdict, run `bundle_dir=${SCHERZO_WORKFLOW_BUNDLE_DIR:-}; if [ -z "$bundle_dir" ]; then bundle_dir="$(cd "$SCHERZO_CONFIG_DIR/workflows" && pwd -P)"; fi; repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$bundle_dir/scripts/scherzo-implementation" plan-completion-context` from the repository root and copy its context values exactly into the JSON artifact.
- The subsequent command gate will fail closed if the JSON is missing, malformed, has verdict `fail`, or has stale context values.

Required verdict artifact:

Write valid JSON (no Markdown fences, no comments, no trailing commas) to `tmp/scherzo-plan-completion-verdict.json` with this schema:

```json
{
  "schema_version": 1,
  "verdict": "pass",
  "blocking_findings": [],
  "evidence": ["Evidence that required behavior and tests are present."],
  "checked_acceptance_criteria": ["Acceptance criterion or required milestone checked."],
  "plan_path": "<PLAN_COMPLETION_PLAN_PATH>",
  "verified_base_change_id": "<PLAN_COMPLETION_BASE_CHANGE_ID>",
  "verified_change_id": "<PLAN_COMPLETION_CHANGE_ID>",
  "verified_diff_fingerprint": "<PLAN_COMPLETION_DIFF_FINGERPRINT>",
  "changed_files": ["<files from PLAN_COMPLETION_CHANGED_FILES>"]
}
```

Use `"verdict": "fail"` when promised behavior is incomplete. In that case, put concrete, actionable missing work in `blocking_findings` so the next agent has one chance to finish it.

Process:

1. Read `tmp/scherzo-implementation.json`, the checked-in review doc, `tmp/execplan-review-doc.md`, `tmp/execplan-implementation-pack.json`, and `tmp/execplan-bundle.json`.
2. Read the implementation response and change analysis above.
3. Inspect changed files/tests only as needed to verify promised behavior and acceptance criteria.
4. Run `bundle_dir=${SCHERZO_WORKFLOW_BUNDLE_DIR:-}; if [ -z "$bundle_dir" ]; then bundle_dir="$(cd "$SCHERZO_CONFIG_DIR/workflows" && pwd -P)"; fi; repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$bundle_dir/scripts/scherzo-implementation" plan-completion-context` and copy the context values exactly.
5. Write `tmp/scherzo-plan-completion-verdict.json`.
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
