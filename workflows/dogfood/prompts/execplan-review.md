Review the ExecPlan review document produced by Scherzo's `workflow:execplan` workflow in the previous step.

Focus only on human-reviewable intent: problem framing, scope, risks, rollout, milestones, and acceptance. The implementation pack is retained as a structured artifact and does not need to appear in the checked-in Markdown. Do not edit files in this step.

Completion-preflight review checklist:
- Flag unverifiable acceptance criteria: acceptance must name commands, tests, artifacts/output, observable behavior, or explicit manual evidence.
- Flag missing test evidence requirements, especially negative/error paths, idempotency, duplicate conflicts, cache/TTL behavior, docs/helper migrations, lint/full validation, and manual/browser/dogfood checks.
- Flag ambiguous milestones that do not say what observable behavior, files, artifacts, or validation will prove completion.
- Flag any required behavior in scope, milestones, or acceptance that is unlikely to be represented in implementation steps or tests.
- Distinguish genuinely late checks: actual code behavior, changed-file coverage, and whether commands passed can still be enforced by the execplan-implementation plan-completion gate.

Final response: list required changes, or say the review doc is ready.
