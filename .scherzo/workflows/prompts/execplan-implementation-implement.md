You are running Scherzo's `workflow:execplan-implementation` workflow for Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue URL:
{{ issue.url }}

Plan preparation output (contains the authoritative `PLAN_PATH`):
{{ steps.prepare_plan.stdout }}

Workflow contract:

- This workflow implements an existing ExecPlan referenced by the Linear issue. The generated Linear issue is only a handoff; `PLAN_PATH` from the preparation output is authoritative.
- You are already inside a dedicated workflow workspace prepared by Scherzo; do not create, forget, finish, switch, push, or otherwise manage workflow workspaces.
- Use `$SCHERZO_WORKSPACE_DRIVER status --human` for source-control inspection.
- Read `.pi/skills/exec-plan/SKILL.md` before implementing and follow its implementation guidance where it does not conflict with this workflow contract.
- The helper output above contains `PLAN_PATH=<path>` and may contain `PLAN_BRIEF_STATUS=ok`, `PLAN_BRIEF_PATH=<path>`, `PLAN_INDEX_PATH=<path>`, and `PLAN_SOURCE_SHA256=<hash>`.
- For this workflow, the generated brief plus named `section` reads are the required first pass after reading the exec-plan skill; the full plan remains authoritative. When `PLAN_BRIEF_STATUS=ok`, read `PLAN_BRIEF_PATH` first, consult `PLAN_INDEX_PATH` when useful, and use `scripts/scherzo-execplan-html section "$PLAN_PATH" "<section>"` for additional named sections. Read the full plan at `PLAN_PATH` when the brief or index is stale, missing, unavailable, truncated for the section you need, inconsistent with `PLAN_SOURCE_SHA256`, or ambiguous.
- New ExecPlans are checked in as Markdown under `docs/plans/*.md`; legacy plans may still be Carbon HTML under `docs/plans/*.html`.
- Keep the ExecPlan as a living document while you work: update its Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective sections when the plan calls for it or when reality differs from the plan. For Markdown plans, edit `PLAN_PATH` directly. For legacy HTML plans, preserve the existing file format; do not convert the checked-in artifact to Markdown. Prefer `scripts/scherzo-execplan-html extract-md "$PLAN_PATH" > tmp/current-execplan.md`, edit that temporary Markdown, render it back with `python3 scripts/scherzo-execplan-html render tmp/current-execplan.md "$PLAN_PATH" "$PLAN_PATH"`, then run `scripts/scherzo-implementation plan-brief --check` and `scripts/scherzo-implementation plan-brief --refresh-if-stale` when implementation metadata exists. Hand-edit generated HTML only as a last resort for a small safe change.
- Do not create VCS commits. The publish step uses the configured workspace driver to publish the final change after review and validation are complete.
- Do not open a PR. The publish step does that after final validation.
- Keep the implementation focused on the plan. Avoid broad refactors and unrelated cleanup.
- Prefer repository-local conventions over generic advice. Read nearby source, tests, scripts, and docs before editing.
- Run targeted validation when useful, but do not duplicate the final full validation unless the plan specifically requires it.

Implementation process:

1. Restate the concrete plan path and implementation target in one or two sentences.
2. Read `PLAN_BRIEF_PATH` when `PLAN_BRIEF_STATUS=ok`, fetch named sections with `scripts/scherzo-execplan-html section "$PLAN_PATH" "<section>"` as needed, and read the full ExecPlan from `PLAN_PATH` when bounded context is stale, missing, unavailable, truncated, inconsistent, or ambiguous.
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
