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

Do not stop early or knowingly submit incomplete work merely because bounded step recovery exists: this `implement_plan` step remains the primary broad implementation pass. Ordinary remaining implementation work is not a permitted terminal state. If known required Progress items, blocking acceptance criteria, or implementation-pack requirements remain now, keep implementing instead of stopping. Submit an incomplete result only when a concrete conflict, missing required handoff artifact, unsafe ambiguity, required product decision, or infrastructure failure prevents further work, and record that condition in `blockers`. If the verifier nevertheless identifies explicit unfinished canonical-plan work after an unblocked partial pass, recovery is authorized to treat those findings as an actionable backlog and complete broad or multi-file work within its configured attempt budget. Treat any repo path in `review_surface_path` or `plan_publication_path` as optional human-review metadata, not as the authoritative implementation input.

Do not create commits, manage workspaces, or open a PR; the workflow publish step owns that.

Required pre-submission acceptance checklist and machine completion submission:

- Before trusting any completion statement, re-read the canonical plan's acceptance sections and inspect the current tree. Treat existing `Progress` checkboxes and `Outcomes & Retrospective` prose as untrusted historical claims until current code, tests, measurements, or commands independently verify them. Never cite those living sections as completion evidence.
- Map every canonical acceptance criterion to one `acceptance_criteria` entry. Quote the criterion (or a stable exact sentence from it), mark `satisfied` truthfully, and attach concrete current-tree `evidence`. Preserve the exact prose; Markdown code-span backticks may be omitted without changing its identity. Code/test/inventory evidence references use a repository-relative path, optionally followed by `:<line>` or `#L<line>`; command evidence includes the exact command; every observation states what was inspected or observed now. Generic format, unit, contract, glinter, and Scherzo lint commands prove regression safety only; command-only evidence cannot establish semantic completion.
- For every plan guardrail that requires reduction, populate `guardrail_checks` with the canonical baseline, current measured value, repository-relative path, and metric (`line_count` or `import_count`). Compare the values explicitly. Never raise or normalize a checked-in ratchet to accommodate a larger current measurement.
- For migrations, clustering, extractions, and replacements, inspect runtime consumers rather than merely the new type declarations. Use `required_references` to prove each new owner record/API is consumed and focused tests directly reference each migrated API and required negative, duplicate, stale, timeout, and idempotent path. Use `absence_checks` to prove replaced legacy fields/helpers are absent from the plan-named files. These are literal current-tree checks run again by Scherzo.
- When the plan requires an unchanged-surface, helper, touched-surface, or implementation inventory, populate `implementation_inventory` explicitly. Give each surface a repository glob, disposition, and evidence; do not replace this inventory with a generic statement that nothing else changed. Inventory patterns use Python `fnmatch` syntax; `|` alternatives and comma-separated `{...}` alternatives are also supported.
- If any criterion lacks current-tree evidence, any required focused path is untested, a legacy field remains, a new owner is declared but unused, a shrinking guardrail misses its baseline, or a required inventory is absent, continue implementing. Only if a concrete blocker prevents continuation should you set `ready_for_verification: false`, name every gap in `remaining_required_work`, and describe the blocker in `blockers`.
- Before your final response, call `submit_implementation_completion` exactly once. A final response without this tool call cannot complete `implement_plan` successfully.
- Set `ready_for_verification: true` only when every `acceptance_criteria` entry is satisfied and the entire required implementation is ready for the unchanged `verify_plan_completion` step. Otherwise set it to `false`.
- Set `changed_files` to the complete sorted repository-relative changed-file list in the workflow diff from the `base_change_id` recorded in `$SCHERZO_RUN_ROOT/state/implementation/metadata.json` through `@` (equivalent to `jj diff --from <base_change_id> --to @ --name-only --color=never`). Scherzo evaluates that same workflow-baseline diff, rather than only the current `@-..@` change, and rejects an empty, fabricated, or stale list.
- Set `remaining_required_work` to every known required plan item that remains. Use an empty list only when none remains.
- Set `blockers` to explicit blockers or conflicts, each with `kind` (`conflict`, `missing_input`, `unsafe_ambiguity`, `required_decision`, `infrastructure`, or `other`) and a concrete `description`. Use an empty list only when unblocked.
- Submit truthful blocker details when a concrete blocker prevents continuation. Never claim readiness merely to pass the gate, and never use `ready_for_verification: false` as a normal milestone checkpoint.
- The completion gate rejects concrete blockers, no-op work, and invalid changed-file evidence before `gate_no_conflict` and `analyze_changes`. If an unblocked pass nevertheless terminates with ordinary remaining work and truthful matching changed-file evidence, Scherzo records it as partial and forwards the current tree to the independent plan-completion verifier; that verifier's bounded recovery treats its fresh findings as the continuation backlog. This safeguard is not permission to stop after a milestone.

Final response:
- Summarize changed files.
- Summarize validation run.
- State whether the submitted implementation is ready for `verify_plan_completion`.
- List known remaining required work as `None` when the verifier should pass, or list the work plus its concrete blocker when further implementation is impossible.
