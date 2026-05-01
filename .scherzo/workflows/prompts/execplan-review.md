You are running the adversarial review step for Scherzo's `workflow:execplan` workflow.

Linear issue:

- Identifier: {{ issue.identifier }}
- Title: {{ issue.title }}
- URL: {{ issue.url }}

Draft validation output:

{{ steps.validate_draft.stdout }}

Workflow contract:

- You are already inside the same dedicated jj workspace as the draft plan. Do not create, forget, finish, switch, push, or otherwise manage jj workspaces.
- Use the repo-local exec-plan-review skill by reading `.pi/skills/exec-plan-review/SKILL.md`.
- Also read `.pi/skills/exec-plan/SKILL.md` because the review skill evaluates against that authoring standard.
- Locate the single changed plan file under `docs/plans/`. The validation output above prints `PLAN_PATH=<path>`.
- Review the plan adversarially for whether it is worth implementing and whether a novice implementer could execute it safely.
- Keep the review bounded: read the two skill files and the plan, then inspect only directly relevant repository files needed to verify serious concerns.
- Do not edit the plan during this step.
- Write the full review to `tmp/execplan-review.md`. `tmp/` is intentionally ignored and must not be part of the PR.
- If there are no material issues, the review should still contain the required review format and a READY verdict.

Dogfood time budget:

- Use at most 10 tool calls before writing `tmp/execplan-review.md`.
- Prefer a useful skeptical review with clear priority fixes over exhaustive repo auditing.

Review process:

1. Read `.pi/skills/exec-plan-review/SKILL.md`.
2. Read `.pi/skills/exec-plan/SKILL.md`.
3. Read the plan from the `PLAN_PATH` printed above, or discover it with `jj diff --from @- --to @ --name-only --color=never` if needed.
4. Apply the review skill's output format exactly.
5. Save the review to `tmp/execplan-review.md`.
6. Finish with a concise response naming the plan path, review path, verdict, and top priority fixes.

Final response format:

## Summary
One short paragraph stating the review verdict and where the review was written.

## Priority fixes
- Top findings that the next workflow step must address, or `None`.
