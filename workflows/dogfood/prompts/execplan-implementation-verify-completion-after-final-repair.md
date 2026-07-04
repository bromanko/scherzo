Verify ExecPlan completion after the final plan-completion repair pass for Scherzo's `workflow:execplan-implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

ExecPlan identity model:

- The workflow task in this prompt is the implementation handoff issue; it owns this implementation run and should be used for Linear/GitHub linkage.
- `$SCHERZO_RUN_ROOT/state/implementation/execplan-bundle.json` records that handoff under `implementation_handoff` and records the source ExecPlan/review-doc issue under `source_issue`.
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

Late recovery classifier output:
{{ steps.classify_plan_completion_gate.stdout }}
{{ steps.classify_plan_completion_gate.stderr }}

Late repair response:
{{ steps.apply_late_plan_completion_feedback.final_response }}

Post-late-repair verifier response:
{{ steps.verify_plan_completion_after_late_repair.final_response }}

Post-late-repair gate output:
{{ steps.gate_plan_completion_after_late_repair.stdout }}
{{ steps.gate_plan_completion_after_late_repair.stderr }}

Final recovery classifier output:
{{ steps.classify_plan_completion_gate_after_late_repair.stdout }}
{{ steps.classify_plan_completion_gate_after_late_repair.stderr }}

Final repair response:
{{ steps.apply_final_plan_completion_feedback.final_response }}

Post-final-repair change analysis output:
{{ steps.analyze_changes_after_final_plan_feedback.stdout }}

Verification contract:

- This is a plan-completion verification, not a code review. Do not critique style, formatting, architecture, or optional polish unless it blocks promised behavior.
- Do not edit tracked source, tests, workflows, docs, the ExecPlan review doc, or plan-completion verdict files. Submit the semantic verdict with the `submit_plan_completion_verdict` structured-output tool instead of writing JSON by hand.
- Before reading prepared ExecPlan files, run `bundle_dir=${SCHERZO_WORKFLOW_BUNDLE_DIR:-}; if [ -z "$bundle_dir" ]; then bundle_dir="$(cd "$SCHERZO_CONFIG_DIR/workflows" && pwd -P)"; fi; "$bundle_dir/scripts/scherzo-implementation" restore-execplan-artifacts` from the repository root. This restores `tmp/` compatibility copies from run-root canonical state if tests or helper fixtures clobbered them.
- Read `$SCHERZO_RUN_ROOT/state/implementation/metadata.json`, `$SCHERZO_RUN_ROOT/state/implementation/execplan-bundle.json`, `$SCHERZO_RUN_ROOT/state/implementation/execplan-review-doc.md`, and `$SCHERZO_RUN_ROOT/state/implementation/execplan-implementation-pack.json`.
- Treat `$SCHERZO_RUN_ROOT/state/implementation/execplan-review-doc.md` as the authoritative canonical plan prepared from the descriptor `plan` entry in `exec_plan_bundle.entries`; metadata `plan_path` points at that local copy.
- Treat the implementation pack as the authoritative mechanical handoff only when it does not conflict with canonical-plan intent, scope, acceptance, safety, or source-plan provenance beyond the expected handoff/source identity split.
- Inspect the canonical plan's Progress, Validation and Acceptance, Milestones, Scope Boundaries, Open Questions, and any explicit non-goals/deferred/stretch sections.
- Compare the final repair summary and changed files/tests against the canonical plan and implementation pack. Inspect the smallest useful set of changed files and tests when the summaries are not enough.
- Treat missing negative/error-path tests, idempotency or duplicate-conflict checks, provider-live/cache coverage, docs/helper migrations, lint/full-validation commands, and required pre-publish manual/browser/dogfood evidence as blocking when the canonical plan or implementation pack requires them and the implementation run does not provide observable evidence.
- Treat unchecked Progress checklist items as evidence requests, not as mandatory source-plan edits. Return `fail` only when the unchecked item corresponds to required behavior, artifacts, tests, validation, or acceptance evidence that is still undelivered or unobservable in the current implementation. Do not fail solely because the immutable source plan still contains historical "implementation pending" or "pack materialization pending" living-status checkboxes when the implementation run provides equivalent evidence.
- Treat explicitly post-implementation manual/browser/dogfood checks as deferred manual verification, not blocking implementation completion. A manual check is explicitly deferred only when the canonical plan or implementation pack says it is performed after implementation, PR publication, or handoff by a human/operator. Do not fail solely because such deferred manual verification has not been completed; record it in `deferred_manual_verification` instead. If a manual check is required before publish and has no deferred timing/owner, it remains blocking when evidence is missing.
- Do not fail for imperfect wording, formatting, or optional/stretch work that is clearly marked optional, stretch, deferred, or out of scope.
- Submit only semantic verdict fields. Do not include `plan_path`, change ids, parent commit ids, diff fingerprints, `changed_files`, or verdict file paths; Scherzo stamps those machine context fields after your tool submission.
- The following checkpoint/gate commands block code review if the structured submission is missing or malformed, if the stamped verdict has verdict `fail`, or if the workspace changes before the gate stamps current context.

Required structured submission:

Call `submit_plan_completion_verdict` with this semantic payload:

```json
{
  "verdict": "pass",
  "blocking_findings": [],
  "evidence": ["Evidence that required behavior and tests are present after the final repair pass."],
  "checked_acceptance_criteria": ["Acceptance criterion or required milestone checked."],
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

Use `"verdict": "fail"` when promised behavior is still incomplete after the final repair/no-op branch. In that case, keep `blocking_findings` concrete and actionable for retained-workspace triage. Set `deferred_manual_verification` to `[]` when there are no explicitly deferred manual checks.

Process:

1. Run the restore command above, then read `$SCHERZO_RUN_ROOT/state/implementation/metadata.json`, the canonical plan at `$SCHERZO_RUN_ROOT/state/implementation/execplan-review-doc.md`, `$SCHERZO_RUN_ROOT/state/implementation/execplan-implementation-pack.json`, and `$SCHERZO_RUN_ROOT/state/implementation/execplan-bundle.json`.
2. Read the verifier, gate, recovery-classifier, repair, and post-repair analysis responses above.
3. Inspect changed files/tests only as needed to verify promised behavior and acceptance criteria.
4. Call `submit_plan_completion_verdict` with the semantic verdict payload. Do not write or edit verdict JSON files.
5. Finish with a concise summary of the verdict and the most important evidence/findings.

Final response format:

## Plan-completion verdict
`pass` or `fail`.

## Blocking findings
- Bullet list, or `None`.

## Evidence checked
- Bullet list of the most important evidence and acceptance criteria checked.

## Artifact
- `plan_completion_verdict_submission` submitted with `submit_plan_completion_verdict`.
