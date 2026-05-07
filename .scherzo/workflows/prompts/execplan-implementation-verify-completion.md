Verify ExecPlan completion for Scherzo's `workflow:execplan-implementation` workflow on Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue description:
{{ issue.description }}

Plan preparation output:
{{ steps.prepare_plan.stdout }}

Implementation step response:
{{ steps.implement_plan.final_response }}

Change analysis output:
{{ steps.analyze_changes.stdout }}

Verification contract:

- This is a plan-completion verification, not a code review. Do not critique style, formatting, architecture, or optional polish unless it blocks promised behavior.
- Do not edit tracked source, tests, workflows, docs, or the ExecPlan. Your only allowed write is the verdict artifact at `tmp/scherzo-plan-completion-verdict.json`.
- The helper output above contains `PLAN_PATH=<path>`. Read the full ExecPlan from that path before deciding.
- Inspect the ExecPlan's Progress, Outcomes & Retrospective, Acceptance Criteria, required milestones, and any explicit non-goals/deferred/stretch sections.
- Compare the implementation summary and changed files/tests against the ExecPlan. Inspect the smallest useful set of changed files and tests when the summary is not enough.
- Explicitly return `fail` for LIV-86-like false successes: required Progress checklist items are still unchecked, required milestones/acceptance criteria are undelivered, or Outcomes says the promised product behavior is not observable.
- Do not fail for imperfect wording, formatting, or optional/stretch work that is clearly marked optional, stretch, deferred, or out of scope.
- Before writing the verdict, run `scripts/scherzo-implementation plan-completion-context` from the repository root and copy its context values exactly into the JSON artifact.
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

1. Read `tmp/scherzo-implementation.json` and the full ExecPlan at `PLAN_PATH`.
2. Read the implementation response and change analysis above.
3. Inspect changed files/tests only as needed to verify promised behavior and acceptance criteria.
4. Run `scripts/scherzo-implementation plan-completion-context` and copy the context values exactly.
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
