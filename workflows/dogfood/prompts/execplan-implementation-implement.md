You are running Scherzo's `workflow:execplan-implementation` workflow for task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

ExecPlan identity model:

- Before implementing, read the repo-local ExecPlan workflow guidance at `.scherzo/workflows/guidance/exec-plan.md`, especially Mode 2: Implementing an ExecPlan. Treat that file as Scherzo's authoritative implementation guidance for this workflow, and do not load or rely on machine-local pi skills, home-directory skill files, `.pi` skill packages, or other machine-local skill paths.
{% include "fragments/execplan-identity-model.md" %}
- Do not report a conflict, fail completion, or request revision solely because the handoff issue, source issue, review doc path, or implementation pack provenance reference different Linear keys. Treat only review-doc/implementation-pack disagreement in intent, scope, acceptance, safety, or source-plan provenance beyond that expected split as blocking.

Use these prepared files as the complete handoff:

- `$SCHERZO_RUN_ROOT/state/implementation/execplan-review-doc.md`: canonical executable plan artifact prepared from the descriptor `plan` entry in `exec_plan_bundle.entries`, including human-reviewable intent, scope, risks, milestones, and acceptance.
- `$SCHERZO_RUN_ROOT/state/implementation/execplan-implementation-pack.json`: mechanical implementation steps, verified facts, tests, interfaces, dependencies, and artifact notes.
- `$SCHERZO_RUN_ROOT/state/implementation/execplan-bundle.json`: retained bundle provenance and hashes.

If intent, scope, acceptance, safety, or source-plan provenance in the canonical plan conflicts with the implementation pack beyond the expected handoff/source identity split described above, write a concise conflict report to `tmp/execplan-conflict.md` and stop without making code changes.

Otherwise implement the entire remaining required scope of the canonical plan. This `implement_plan` step is the workflow's broad implementation pass before the semantic plan-completion verifier; a normal exit from this step is interpreted as "ready for `verify_plan_completion`", not "one milestone completed." Treat milestones as internal checkpoints: complete the next required milestone, run its validation, update the ExecPlan living sections as required, then continue to the next required milestone. Do not final-answer merely because one milestone passes. Final-answer only when all required behavior, tests, validation, docs/helper migrations, artifacts, and acceptance evidence needed for the unchanged plan-completion verifier should be present.

Do not rely on step recovery to finish broad remaining plan work. Recovery is intentionally narrow and may only attempt the smallest safe local repair for explicit verifier findings. If known required Progress items, blocking acceptance criteria, or implementation-pack requirements remain, keep implementing instead of stopping, unless you are blocked by an explicit conflict, missing required handoff artifact, unsafe ambiguity, or required product decision. Treat any repo path in `review_surface_path` or `plan_publication_path` as optional human-review metadata, not as the authoritative implementation input.

Do not create commits, manage workspaces, or open a PR; the workflow publish step owns that.

Final response:
- Summarize changed files.
- Summarize validation run.
- State whether the implementation is ready for `verify_plan_completion`.
- List known remaining required work as `None` when the verifier should pass, or list concrete blockers if the verifier should not pass.
