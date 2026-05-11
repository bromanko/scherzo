You are running Scherzo's `workflow:execplan-implementation` workflow for Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue URL:
{{ issue.url }}

Plan preparation output (contains the authoritative `PLAN_PATH`):
{{ steps.prepare_plan.stdout }}

Workflow contract:

- This workflow implements an existing ExecPlan referenced by the Linear issue. The generated Linear issue is only a handoff; `PLAN_PATH` from the preparation output is authoritative.
- You are already inside a dedicated jj workspace created by Scherzo; do not create, forget, finish, switch, push, or otherwise manage jj workspaces.
- Use `jj status --color=never` for source-control inspection.
- Read `.pi/skills/exec-plan/SKILL.md` before implementing and follow its implementation guidance where it does not conflict with this workflow contract.
- The helper output above contains `PLAN_PATH=<path>`. Read that plan artifact completely before editing anything. New ExecPlans may be checked in as Carbon HTML under `docs/plans/*.html`; older plans may still be Markdown.
- Keep the ExecPlan as a living document while you work: update its Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective sections when the plan calls for it or when reality differs from the plan. Preserve the plan's existing file format; do not convert HTML plans back to Markdown.
- Do not create jj/git commits. The publish step creates the final logical jj commit after review and validation are complete.
- Do not open a PR. The publish step does that after final validation.
- Keep the implementation focused on the plan. Avoid broad refactors and unrelated cleanup.
- Prefer repository-local conventions over generic advice. Read nearby source, tests, scripts, and docs before editing.
- Run targeted validation when useful, but do not duplicate the final full validation unless the plan specifically requires it.

Implementation process:

1. Restate the concrete plan path and implementation target in one or two sentences.
2. Read the full ExecPlan from `PLAN_PATH` and inspect the smallest useful set of files it names.
3. Implement the plan in small, safe increments.
4. Add or update the tests required by the plan.
5. Update the ExecPlan's living-document sections so the current state is recoverable from the plan alone.
6. Run targeted validation if it is cheap and relevant.
7. Finish with a concise summary of changed files and validation you ran.

Final response format:

## Summary
What you implemented and why.

## Files changed
- `path`: short note.

## Validation
- Commands you ran, or `Not run; deferred to workflow validation`.

## Notes for review
Anything the review step should know.
