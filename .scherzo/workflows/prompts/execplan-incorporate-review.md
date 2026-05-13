You are running the review-incorporation step for Scherzo's `workflow:execplan` workflow.

Linear issue:

- Identifier: {{ issue.identifier }}
- Title: {{ issue.title }}
- URL: {{ issue.url }}

Initial draft validation status: {{ steps.validate_draft.status }}
Initial draft validation stdout:

{{ steps.validate_draft.stdout }}

Initial draft validation stderr:

{{ steps.validate_draft.stderr }}

Repair step response:

{{ steps.repair_validation.final_response }}

Final validation stdout:

{{ steps.validate_after_repair.stdout }}

Final validation stderr:

{{ steps.validate_after_repair.stderr }}

Review step response:

{{ steps.review_plan.final_response }}

Workflow contract:

- You are already inside the same dedicated workflow workspace prepared by Scherzo as the draft plan. Do not create, forget, finish, switch, push, or otherwise manage workflow workspaces.
- Use the repo-local exec-plan skill by reading `.pi/skills/exec-plan/SKILL.md` before revising.
- Use the repo-local exec-plan-review skill by reading `.pi/skills/exec-plan-review/SKILL.md` before interpreting the review.
- Read `tmp/execplan-review.md` and the Markdown plan artifact under `docs/plans/`.
- Keep the incorporation bounded: focus on the review findings and inspect only files needed to resolve concrete uncertainty.
- Revise only the `docs/plans/*.md` plan artifact. Do not create a tracked HTML artifact, and do not edit source code, tests, config, existing docs, or the review file.
- Incorporate every BLOCKING and GAP finding unless it is demonstrably inapplicable. If you reject a finding, record the rationale in the plan's `## Decision Log`.
- Consider SUGGESTION findings and incorporate the ones that materially improve safety, executability, or scope clarity.
- Preserve the ExecPlan as a living document and keep all sections self-contained.
- If any stakeholder input remains unresolved, keep explicit `[CLARIFY]` tags and list each item in `## Open Questions and Clarifications Needed`.
- Include `## Open Questions and Clarifications Needed` even when there are no open questions; write `None.` in that case.
- Use repository-relative paths only. Do not introduce absolute local paths, even as examples or negative test data. Do not include literal prefixes such as `/Users/`, `/home/`, `/private/`, or `/var/folders/`; use placeholders like `<absolute-local-path>` when discussing forbidden path shapes.

Dogfood time budget:

- Use at most 10 tool calls before updating the plan.
- If a review finding requires stakeholder input, record it as `[CLARIFY]` rather than continuing open-ended investigation.

Incorporation process:

1. Read both repo-local skill files.
2. Read the Markdown plan path printed as `PLAN_PATH=<path>` in the final validation stdout above, or discover it with `$SCHERZO_WORKSPACE_DRIVER changed-files --json` if needed.
3. Read `tmp/execplan-review.md`.
4. Update the plan so a later implementation workflow can start from the plan alone.
5. Re-read the final plan for consistency across purpose, scope, milestones, concrete steps, tests, validation, rollout, risks, and open questions.
6. Finish with a concise response naming the plan file and summarizing review changes made.

Final response format:

## Summary
One short paragraph stating that the reviewed Markdown ExecPlan artifact was updated.

## Incorporated changes
- Bullet list of the most important review findings addressed.

## Open questions
- `None` or the remaining `[CLARIFY]` items also recorded in the plan.
