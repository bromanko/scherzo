You are running Scherzo's `workflow:execplan` workflow for this Linear issue.

Linear issue:

- Identifier: {{ issue.identifier }}
- Title: {{ issue.title }}
- URL: {{ issue.url }}
- State: {{ issue.state }}
- Priority: {{ issue.priority }}
- Labels: {% for label in issue.labels %}{{ label }} {% endfor %}

Description:

{{ issue.description }}

Workflow contract:

- This workflow turns a sufficiently detailed Linear ticket into a checked-in ExecPlan proposal.
- You are already inside a dedicated jj workspace created by Scherzo; do not create, forget, finish, switch, push, or otherwise manage jj workspaces.
- Use `jj status --color=never` for source-control inspection.
- Use the repo-local exec-plan skill by reading `.pi/skills/exec-plan/SKILL.md` before drafting.
- Inspect the current repository with the smallest useful scope required to make the plan accurate and self-contained.
- The ticket should contain enough information to draft the plan. Prefer recording `[CLARIFY]` items over exhaustive discovery when details are ambiguous.
- Do not read existing `docs/plans/*.md` files except to avoid filename collisions; use the skill skeleton rather than prior plans as the format source.
- Do not implement the ticket. Do not edit source code, tests, config, or existing docs except for the one new plan file.
- Create exactly one Markdown ExecPlan under `docs/plans/`.
- Name the file `docs/plans/{{ issue.identifier }}-<short-kebab-title>.md`, using a lowercase title slug.
- The plan must be self-contained and portable. Use repository-relative paths only; never write absolute local paths.
- If the ticket lacks information needed to close a design choice, still produce the best plan you can. Mark uncertainty with `[CLARIFY]` and include it in `## Open Questions and Clarifications Needed`.
- Include `## Open Questions and Clarifications Needed` even when there are no open questions; write `None.` in that case.
- Keep the plan suitable for review in a PR. It should be detailed enough to implement later, but it must not start implementation now.

Dogfood time budget:

- Use at most 12 tool calls before writing the draft plan.
- If the relevant facts are still uncertain at that point, write the best plan possible and mark uncertainty with `[CLARIFY]`.
- After writing the plan file, stop searching and finish the step.

Drafting process:

1. Read `.pi/skills/exec-plan/SKILL.md` and follow its authoring guidance.
2. Restate the problem from the Linear ticket in operator/user terms.
3. Inspect only the repository files needed to make file paths, commands, existing behavior, and validation steps credible.
4. Write the plan to the single `docs/plans/{{ issue.identifier }}-<short-kebab-title>.md` file.
5. Run lightweight local checks only if useful for fact-finding. Do not run broad validation unless needed to verify facts in the plan.
6. Finish with a concise response naming the plan file and any `[CLARIFY]` items.

Final response format:

## Summary
One short paragraph stating where the draft ExecPlan was written.

## Evidence
- Bullet list of the key repository files, commands, or facts inspected.

## Open questions
- `None` or the list of `[CLARIFY]` items also recorded in the plan.
