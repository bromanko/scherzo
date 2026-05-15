Verify ExecPlan completion for Scherzo's `workflow:execplan-implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

Plan preparation output (contains the authoritative `PLAN_PATH`):
{{ steps.prepare_plan.stdout }}

Implementation step response:
{{ steps.implement_plan.final_response }}

Change analysis output:
{{ steps.analyze_changes.stdout }}

Verification contract:

- This is a plan-completion verification, not a code review. Do not critique style, formatting, architecture, or optional polish unless it blocks promised behavior.
- Do not edit tracked source, tests, workflows, docs, or the ExecPlan. Your only allowed write is the verdict artifact at `tmp/scherzo-plan-completion-verdict.json`.
- The helper output above contains `PLAN_PATH=<path>` and may contain `PLAN_BRIEF_STATUS=ok`, `PLAN_BRIEF_PATH=<path>`, `PLAN_INDEX_PATH=<path>`, and `PLAN_SOURCE_SHA256=<hash>`. For this workflow, the generated brief plus named `section` reads are the required first pass before deciding; the full plan remains authoritative fallback.
- Read `tmp/scherzo-implementation.json`. When brief metadata is present, run `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-implementation" plan-brief --check`; if it reports stale or unavailable, run `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-implementation" plan-brief --refresh-if-stale` or fall back to the full plan if refresh fails. Read `PLAN_BRIEF_PATH` when available, inspect `PLAN_INDEX_PATH` when useful, and use `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-execplan-html" section "$PLAN_PATH" "<section>"` for Progress, Outcomes & Retrospective, Acceptance Criteria if present, Milestones, Testing and Falsifiability, Validation and Acceptance, Scope Boundaries, Open Questions, and any truncated or missing section. Read the full ExecPlan at `PLAN_PATH` when bounded context is stale, missing, unavailable, truncated, inconsistent with `PLAN_SOURCE_SHA256`, or ambiguous.
- Inspect the ExecPlan's Progress, Outcomes & Retrospective, Acceptance Criteria, required milestones, and any explicit non-goals/deferred/stretch sections from the brief, named sections, or full-plan fallback.
- Compare the implementation summary and changed files/tests against the ExecPlan. Inspect the smallest useful set of changed files and tests when the summary is not enough.
- Explicitly return `fail` for LIV-86-like false successes: required Progress checklist items are still unchecked, required milestones/acceptance criteria are undelivered, or Outcomes says the promised product behavior is not observable.
- Do not fail for imperfect wording, formatting, or optional/stretch work that is clearly marked optional, stretch, deferred, or out of scope.
- Before writing the verdict, run `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-implementation" plan-completion-context` from the repository root and copy its context values exactly into the JSON artifact.
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

1. Read `tmp/scherzo-implementation.json`, check or refresh brief freshness when brief metadata exists, read `PLAN_BRIEF_PATH` when available, and fetch named sections or the full ExecPlan at `PLAN_PATH` when bounded context is insufficient.
2. Read the implementation response and change analysis above.
3. Inspect changed files/tests only as needed to verify promised behavior and acceptance criteria.
4. Run `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-implementation" plan-completion-context` and copy the context values exactly.
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
