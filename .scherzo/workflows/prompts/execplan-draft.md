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

- This workflow turns a sufficiently detailed Linear ticket into a checked-in Markdown ExecPlan proposal.
- You are already inside a dedicated workflow workspace prepared by Scherzo; do not create, forget, finish, switch, push, or otherwise manage workflow workspaces.
- Use `$SCHERZO_WORKSPACE_DRIVER status --human` for source-control inspection.
- Use the repo-local exec-plan skill by reading `.pi/skills/exec-plan/SKILL.md` before drafting.
- Inspect the current repository with the smallest useful scope required to make the plan accurate and self-contained.
- The ticket should contain enough information to draft the plan. Prefer recording `[CLARIFY]` items over exhaustive discovery when details are ambiguous.
- Do not read existing `docs/plans/*` files except to avoid filename collisions; use the skill skeleton for the plan content requirements.
- Do not implement the ticket. Do not edit source code, tests, config, or existing docs except for the one new plan artifact.
- Create exactly one self-contained Markdown ExecPlan source file under `docs/plans/`.
- Name the file `docs/plans/{{ issue.identifier }}-<short-kebab-title>.md`, using a lowercase title slug.
- Do not create a tracked `docs/plans/*.html` file. HTML is a derived viewer artifact only, not the source of truth. If a local preview is useful, render it outside the tracked plan path or under ignored `tmp/`.
- Author the final tracked artifact directly as Markdown. The plan must start with a level-1 `#` title, contain the required living-document sections, contain a Progress checklist, and include `## Open Questions and Clarifications Needed`.
- The plan must be self-contained and portable. Use repository-relative paths only; never write absolute local paths, even as examples or negative test data. Do not include literal prefixes such as `/Users/`, `/home/`, `/private/`, or `/var/folders/`; use placeholders like `<absolute-local-path>` when discussing forbidden path shapes.
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
4. Write the plan content as the single `docs/plans/{{ issue.identifier }}-<short-kebab-title>.md` Markdown source artifact.
5. Run `scripts/scherzo-execplan validate docs/plans/{{ issue.identifier }}-<short-kebab-title>.md` if practical; otherwise rely on the following validation step.
6. Run lightweight local checks only if useful for fact-finding. Do not run broad validation unless needed to verify facts in the plan.
7. Finish with a concise response naming the Markdown plan artifact and any `[CLARIFY]` items.

Final response format:

## Summary
One short paragraph stating where the draft Markdown ExecPlan artifact was written.

## Evidence
- Bullet list of the key repository files, commands, or facts inspected.

## Open questions
- `None` or the list of `[CLARIFY]` items also recorded in the plan.
