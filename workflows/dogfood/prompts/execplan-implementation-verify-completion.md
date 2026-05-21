Verify ExecPlan completion for Scherzo's `workflow:execplan-implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

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

Change analysis output:
{{ steps.analyze_changes.stdout }}

Verification contract:

- This is a plan-completion verification, not a code review. Do not critique style, formatting, architecture, or optional polish unless it blocks promised behavior.
- Do not edit tracked source, tests, workflows, docs, or the ExecPlan review doc. Your only allowed write is the verdict artifact at `tmp/scherzo-plan-completion-verdict.json`.
- Before reading prepared ExecPlan files, run `bundle_dir=${SCHERZO_WORKFLOW_BUNDLE_DIR:-}; if [ -z "$bundle_dir" ]; then bundle_dir="$(cd "$SCHERZO_CONFIG_DIR/workflows" && pwd -P)"; fi; "$bundle_dir/scripts/scherzo-implementation" restore-execplan-artifacts` from the repository root. This restores `tmp/` compatibility copies from run-root canonical state if tests or helper fixtures clobbered them.
- Read `tmp/scherzo-implementation.json`, `tmp/execplan-bundle.json`, `tmp/execplan-review-doc.md`, and `tmp/execplan-implementation-pack.json`.
- Treat `tmp/execplan-review-doc.md` as the authoritative canonical plan resolved during prepare from `exec_plan_bundle.plan.ref` (or legacy `review_doc.path` fallback). `tmp/scherzo-implementation.json` `plan_path` points at that prepared local plan; any `review_surface_path` or legacy `review_doc.path` is optional publication metadata.
- Treat the implementation pack as the authoritative mechanical handoff only when it does not conflict with canonical-plan intent, scope, acceptance, safety, or source-plan provenance beyond the expected handoff/source identity split.
- Inspect the canonical plan's Progress, Validation and Acceptance, Milestones, Scope Boundaries, Open Questions, and any explicit non-goals/deferred/stretch sections.
- Compare the implementation summary and changed files/tests against the canonical plan and implementation pack. Inspect the smallest useful set of changed files and tests when the summary is not enough.
- Treat unchecked Progress checklist items as evidence requests, not as mandatory source-plan edits. Return `fail` only when the unchecked item corresponds to required behavior, artifacts, tests, validation, or acceptance evidence that is still undelivered or unobservable in the current implementation. Do not fail solely because the immutable source plan still contains historical "implementation pending" or "pack materialization pending" living-status checkboxes when the implementation run provides equivalent evidence.
- Treat explicitly post-implementation manual/browser/dogfood checks as deferred manual verification, not blocking implementation completion. A manual check is explicitly deferred only when the canonical plan or implementation pack says it is performed after implementation, PR publication, or handoff by a human/operator. Do not fail solely because such deferred manual verification has not been completed; record it in `deferred_manual_verification` instead. If a manual check is required before publish and has no deferred timing/owner, it remains blocking when evidence is missing.
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
  "changed_files": ["<files from PLAN_COMPLETION_CHANGED_FILES>"],
  "deferred_manual_verification": [
    {
      "check": "Manual/browser/dogfood check still to perform after implementation.",
      "reason": "Requires human/operator environment after implementation.",
      "owner": "operator",
      "when": "after implementation workflow"
    }
  ]
}
```

Use `"verdict": "fail"` when promised behavior is incomplete. In that case, put concrete, actionable missing work in `blocking_findings` so the next agent has one chance to finish it. Set `deferred_manual_verification` to `[]` when there are no explicitly deferred manual checks.

Process:

1. Run the restore command above, then read `tmp/scherzo-implementation.json`, the canonical plan at `tmp/execplan-review-doc.md`, `tmp/execplan-implementation-pack.json`, and `tmp/execplan-bundle.json`.
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
