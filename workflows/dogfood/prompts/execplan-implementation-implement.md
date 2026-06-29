You are running Scherzo's `workflow:execplan-implementation` workflow for task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

ExecPlan identity model:

- Before implementing, read the repo-local ExecPlan workflow guidance at `.scherzo/workflows/guidance/exec-plan.md`, especially Mode 2: Implementing an ExecPlan. Treat that file as Scherzo's authoritative implementation guidance for this workflow, and do not load or rely on machine-local pi skills, home-directory skill files, `.pi` skill packages, or other machine-local skill paths.
- The workflow task in this prompt is the implementation handoff issue; it owns this implementation run and should be used for Linear/GitHub linkage.
- `tmp/execplan-bundle.json` records that handoff under `implementation_handoff` and records the source ExecPlan/review-doc issue under `source_issue`.
- `implementation_handoff.issue_identifier` may differ from `source_issue.identifier`; that split is valid and expected for handoff tasks.
- Do not report a conflict, fail completion, or request revision solely because the handoff issue, source issue, review doc path, or implementation pack provenance reference different Linear keys. Treat only review-doc/implementation-pack disagreement in intent, scope, acceptance, safety, or source-plan provenance beyond that expected split as blocking.

Use these prepared files as the complete handoff:

- `tmp/execplan-review-doc.md`: canonical executable plan artifact resolved from descriptor-first `plan` entry in `exec_plan_bundle.entries` (or legacy `exec_plan_bundle.plan.ref` / `review_doc.path` fallback), including human-reviewable intent, scope, risks, milestones, and acceptance.
- `tmp/execplan-implementation-pack.json`: mechanical implementation steps, verified facts, tests, interfaces, dependencies, and artifact notes.
- `tmp/execplan-bundle.json`: retained bundle provenance and hashes.

If intent, scope, acceptance, safety, or source-plan provenance in the canonical plan conflicts with the implementation pack beyond the expected handoff/source identity split described above, write a concise conflict report to `tmp/execplan-conflict.md` and stop without making code changes. Otherwise implement the next required milestone. Treat any repo path in `review_surface_path` or legacy `review_doc.path` as optional human-review metadata, not as the authoritative implementation input.

Do not create commits, manage workspaces, or open a PR; the workflow publish step owns that.

Final response: summarize changed files and validation run.
