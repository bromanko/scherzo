Verify ExecPlan completion for Scherzo's `workflow:execplan-implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

ExecPlan identity model:

{% include "fragments/execplan-identity-model.md" %}
- Do not report a conflict, fail completion, or request revision solely because the handoff issue, source issue, review doc path, or implementation pack provenance reference different Linear keys. Treat only review-doc/implementation-pack disagreement in intent, scope, acceptance, safety, or source-plan provenance beyond that expected split as blocking.

{% include "fragments/execplan-verification-contract.md" %}

- Read `$SCHERZO_RUN_ROOT/state/implementation/metadata.json`, `$SCHERZO_RUN_ROOT/state/implementation/execplan-bundle.json`, `$SCHERZO_RUN_ROOT/state/implementation/execplan-review-doc.md`, `$SCHERZO_RUN_ROOT/state/implementation/execplan-implementation-pack.json`, and `$SCHERZO_RUN_ROOT/state/implementation/scherzo-implementation-completion-diagnostic.json` when present.
- Treat `$SCHERZO_RUN_ROOT/state/implementation/execplan-review-doc.md` as the authoritative canonical plan. Treat the implementation pack as authoritative mechanical handoff only when it does not conflict with canonical-plan intent, scope, acceptance, safety, or source-plan provenance beyond the expected handoff/source identity split.
- Inspect the canonical plan's Progress, Validation and Acceptance, Milestones, Scope Boundaries, and Open Questions.
- Inspect the smallest useful set of changed files and tests needed to verify promised behavior. Use current workspace evidence rather than historical prompt transcripts.
- When the implementation-completion diagnostic has `status: partial`, reconcile every submitted unsatisfied criterion and `remaining_required_work` item with the current tree. Put each still-valid gap in `blocking_findings` so bounded recovery receives an actionable continuation backlog. Do not fail solely because the diagnostic says partial, and do not ignore its gaps merely because the agent made some changes.
- Treat missing negative/error-path tests, idempotency or duplicate-conflict checks, provider-live/cache coverage, docs/helper migrations, lint/full-validation commands, and required pre-publish manual/browser/dogfood evidence as blocking when the canonical plan or implementation pack requires them and the implementation run does not provide observable evidence.
- Treat unchecked Progress checklist items as evidence requests, not mandatory plan edits. Return `fail` only when required behavior, tests, validation, artifacts, or acceptance evidence is still missing or unobservable.
- Treat explicitly post-implementation manual/browser/dogfood checks as `deferred_manual_verification`, not blocking completion, when the canonical plan or implementation pack says a human/operator performs them after implementation, PR publication, or handoff.
- Submit only semantic verdict fields. Do not include plan paths, verdict file paths, changed files, change ids, parent commit ids, or diff fingerprints; the same-step command validator stamps those machine fields.
- The same-step command validator runs `gate-plan-completion --from-submission`, stamps machine context into the canonical verdict artifact, and fails this step on `verdict: fail`. Because `validation_retries` is `0`, a fail verdict hands control directly to step recovery instead of an in-session retry.
- Treat post-implementation manual/browser/dogfood evidence as deferred manual verification when the plan marks it for operator follow-up.

Required semantic submission shape:

Call `submit_plan_completion_verdict` with:

```json
{
  "verdict": "pass",
  "blocking_findings": [],
  "evidence": ["Evidence that required behavior and tests are present."],
  "checked_acceptance_criteria": ["Acceptance criterion checked."],
  "deferred_manual_verification": [
    {
      "check": "post-implementation manual/browser/dogfood verification",
      "reason": "Requires a human/operator environment after implementation.",
      "owner": "operator",
      "when": "after implementation workflow"
    }
  ]
}
```

Use `verdict: fail` when promised behavior is incomplete. Put concrete, actionable missing work in `blocking_findings` so the recovery worker can attempt the smallest safe repair.

Process:

1. Read `$SCHERZO_RUN_ROOT/state/implementation/metadata.json`, the canonical plan at `$SCHERZO_RUN_ROOT/state/implementation/execplan-review-doc.md`, `$SCHERZO_RUN_ROOT/state/implementation/execplan-implementation-pack.json`, `$SCHERZO_RUN_ROOT/state/implementation/execplan-bundle.json`, and the implementation-completion diagnostic when present.
2. Check the current workspace and changed files/tests against the canonical plan and implementation pack. Reconcile any partial diagnostic backlog against current-tree evidence.
3. Decide whether required behavior, tests, validation, docs/helper migrations, and acceptance evidence are present; return concrete blocking findings for every partial backlog item that remains valid.
4. Submit only semantic verdict fields with `submit_plan_completion_verdict`.
5. Finish with a concise verdict summary.


Final response format:

## Plan-completion verdict
`pass` or `fail`.

## Blocking findings
- Bullet list, or `None`.

## Evidence checked
- Bullet list of the most important evidence and acceptance criteria checked.

## Artifact
- `plan_completion_verdict_submission` submitted with `submit_plan_completion_verdict`.
