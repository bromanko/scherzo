Verify final ExecPlan completion before final validation and publish for Scherzo's `workflow:execplan-implementation` workflow on Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue URL:
{{ issue.url }}

Plan preparation output (contains the authoritative `PLAN_PATH`):
{{ steps.prepare_plan.stdout }}

Initial implementation step response:
{{ steps.implement_plan.final_response }}

Pre-review plan-completion gate output:
{{ steps.gate_plan_completion.stdout }}

Review summary:
{{ steps.review_changes.final_response }}

Review feedback application response:
{{ steps.apply_review_feedback.final_response }}

Base refresh output before validation:
{{ steps.refresh_base_before_validation.stdout }}

Validation result after refresh:
- `validate_after_refresh` exit code: {{ steps.validate_after_refresh.exit_code }}
- Structured validation artifact: `tmp/scherzo-implementation-validation.json`
- If the exit code is `0`, treat validation as passed without reading or quoting full SelfCI stdout unless an unexpected inconsistency needs investigation.
- If the exit code is nonzero, read `failure_summary`, `stdout_excerpt`, and `stderr_excerpt` from `tmp/scherzo-implementation-validation.json`; those fields are bounded. Full stdout/stderr remains available in `.scherzo/command-step-diagnostics/validate_after_refresh.txt` in the retained workspace when available.

Base-drift repair response:
{{ steps.repair_base_drift.final_response }}

Verification contract:

- This is a final plan-completion verification, not a code review. Do not critique style, formatting, architecture, or optional polish unless it blocks promised behavior.
- Do not edit tracked source, tests, workflows, docs, or the ExecPlan. Your only allowed write is replacing the verdict artifact at `tmp/scherzo-plan-completion-verdict.json`.
- The helper output above contains `PLAN_PATH=<path>`. Read the full ExecPlan from that path before deciding.
- Verify the current patch after review feedback, base refresh, and any base-drift repair. Do not rely only on the pre-review verdict.
- Inspect the ExecPlan's Progress, Outcomes & Retrospective, Acceptance Criteria, required milestones, and any explicit non-goals/deferred/stretch sections.
- Compare the final changed files/tests against the ExecPlan. Inspect the smallest useful set of current changed files and tests when the summaries are not enough.
- Explicitly return `fail` for LIV-86-like false successes: required Progress checklist items are still unchecked, required milestones/acceptance criteria are undelivered, or Outcomes says the promised product behavior is not observable.
- Do not fail for imperfect wording, formatting, or optional/stretch work that is clearly marked optional, stretch, deferred, or out of scope.
- Before writing the verdict, run `scripts/scherzo-implementation plan-completion-context` from the repository root and copy its context values exactly into the JSON artifact.
- The workflow runs final validation after this verifier. The following final command gate blocks publish if this verdict is missing, malformed, stale relative to the validated patch, or `fail`.

Required verdict artifact:

Write valid JSON (no Markdown fences, no comments, no trailing commas) to `tmp/scherzo-plan-completion-verdict.json` with this schema:

```json
{
  "schema_version": 1,
  "verdict": "pass",
  "blocking_findings": [],
  "evidence": ["Evidence that required behavior and tests are present in the final patch."],
  "checked_acceptance_criteria": ["Acceptance criterion or required milestone checked."],
  "plan_path": "<PLAN_COMPLETION_PLAN_PATH>",
  "verified_base_change_id": "<PLAN_COMPLETION_BASE_CHANGE_ID>",
  "verified_change_id": "<PLAN_COMPLETION_CHANGE_ID>",
  "verified_diff_fingerprint": "<PLAN_COMPLETION_DIFF_FINGERPRINT>",
  "changed_files": ["<files from PLAN_COMPLETION_CHANGED_FILES>"]
}
```

Use `"verdict": "fail"` when promised behavior is incomplete in the final current patch. In that case, keep `blocking_findings` concrete and actionable for retained-workspace triage; there is no further automatic implementation repair chance in this workflow run.

Process:

1. Read `tmp/scherzo-implementation.json` and the full ExecPlan at `PLAN_PATH`.
2. Read the review/review-feedback and base-refresh/repair summaries above. Inspect `tmp/scherzo-implementation-validation.json` only when the validation exit code or repair response needs clarification.
3. Inspect current changed files/tests only as needed to verify promised behavior and acceptance criteria.
4. Run `scripts/scherzo-implementation plan-completion-context` and copy the context values exactly.
5. Replace `tmp/scherzo-plan-completion-verdict.json` with the final validation-time verdict.
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
