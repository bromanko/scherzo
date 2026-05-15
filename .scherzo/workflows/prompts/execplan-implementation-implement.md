You are running Scherzo's `workflow:execplan-implementation` workflow for Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue URL:
{{ issue.url }}

Plan preparation output (contains the authoritative `PLAN_PATH`):
{{ steps.prepare_plan.stdout }}

Workflow contract:

- This workflow implements an existing ExecPlan referenced by the Linear issue. The generated Linear issue is only a handoff; `PLAN_PATH` from the preparation output is authoritative.
- You are already inside a dedicated workflow workspace prepared by Scherzo; do not create, forget, finish, switch, push, or otherwise manage workflow workspaces.
- Use `$SCHERZO_WORKSPACE_DRIVER status --human` for source-control inspection.
- Follow the workflow-packaged ExecPlan implementation standard in this prompt. Do not require a local Pi skill file; all guidance needed for this workflow step is embedded below.
- The helper output above contains `PLAN_PATH=<path>` and may contain `PLAN_BRIEF_STATUS=ok`, `PLAN_BRIEF_PATH=<path>`, `PLAN_INDEX_PATH=<path>`, and `PLAN_SOURCE_SHA256=<hash>`.
- For this workflow, the generated brief plus named `section` reads are the required first pass; the full plan remains authoritative. When `PLAN_BRIEF_STATUS=ok`, read `PLAN_BRIEF_PATH` first, consult `PLAN_INDEX_PATH` when useful, and use `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-execplan-html" section "$PLAN_PATH" "<section>"` for additional named sections. Read the full plan at `PLAN_PATH` when the brief or index is stale, missing, unavailable, truncated for the section you need, inconsistent with `PLAN_SOURCE_SHA256`, or ambiguous.
- New ExecPlans are checked in as Markdown under `docs/plans/*.md`; legacy plans may still be Carbon HTML under `docs/plans/*.html`.
- Keep the ExecPlan as a living document while you work: update its Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective sections when the plan calls for it or when reality differs from the plan. For Markdown plans, edit `PLAN_PATH` directly. For legacy HTML plans, preserve the existing file format; do not convert the checked-in artifact to Markdown. Prefer `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-execplan-html" extract-md "$PLAN_PATH" > tmp/current-execplan.md`, edit that temporary Markdown, render it back with `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-execplan-html" render tmp/current-execplan.md "$PLAN_PATH" "$PLAN_PATH"`, then run `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-implementation" plan-brief --check` and `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-implementation" plan-brief --refresh-if-stale` when implementation metadata exists. Hand-edit generated HTML only as a last resort for a small safe change.
- Do not create VCS commits. The publish step uses the configured workspace driver to publish the final change after review and validation are complete.
- Do not open a PR. The publish step does that after final validation.
- Keep the implementation focused on the plan. Avoid broad refactors and unrelated cleanup.
- Prefer repository-local conventions over generic advice. Read nearby source, tests, scripts, and docs before editing.
- Run targeted validation when useful, but do not duplicate the final full validation unless the plan specifically requires it.

Workflow-packaged ExecPlan implementation standard:

- Treat `PLAN_PATH` as the living, self-contained source of truth for the implementation. Understand the purpose, milestones, concrete steps, risks, validation commands, and acceptance criteria before making code changes; use the bounded brief and section reads first, with full-plan fallback as described above.
- Proceed autonomously through the next plan milestone. Resolve minor ambiguity with the safest reasonable interpretation, and record non-trivial reasoning in the plan's Decision Log rather than asking for generic next-step guidance.
- Keep the plan current at every stopping point. Update Progress for completed and remaining work, record unexpected facts in Surprises & Discoveries, record design choices in Decision Log, and add Outcomes & Retrospective entries at major milestones or completion.
- Validate risky assumptions early. If the plan calls for a spike, prototype, migration checkpoint, or red/green test sequence, follow it before making broader changes.
- Maintain self-containment. Every plan edit must leave enough context for another agent to resume from the plan alone, using repository-relative paths only.
- Treat plan commit points as logical checkpoints only. This workflow's deterministic publish step owns commits and PR creation, so do not create commits yourself.

Implementation process:

1. Restate the concrete plan path and implementation target in one or two sentences.
2. Read `PLAN_BRIEF_PATH` when `PLAN_BRIEF_STATUS=ok`, fetch named sections with `repo_root=${SCHERZO_REPO_ROOT:-$(cd "$SCHERZO_CONFIG_DIR/.." && pwd -P)}; "$repo_root/scripts/scherzo-execplan-html" section "$PLAN_PATH" "<section>"` as needed, and read the full ExecPlan from `PLAN_PATH` when bounded context is stale, missing, unavailable, truncated, inconsistent, or ambiguous.
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
