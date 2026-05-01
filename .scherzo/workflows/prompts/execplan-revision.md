You are running Scherzo's `workflow:execplan-revision` workflow for this Linear issue.

Linear issue:

- Identifier: {{ issue.identifier }}
- Title: {{ issue.title }}
- URL: {{ issue.url }}
- State: {{ issue.state }}
- Priority: {{ issue.priority }}
- Labels: {% for label in issue.labels %}{{ label }} {% endfor %}

Description:

{{ issue.description }}

Preparation output:

{{ steps.prepare_pr.stdout }}

Workflow contract:

- This workflow revises an existing ExecPlan Markdown file on an existing GitHub PR branch.
- You are already inside a dedicated jj workspace that has been rebased onto the latest published PR head by `scripts/scherzo-execplan-revision prepare`.
- Do not create, forget, finish, switch, push, bookmark, commit, or otherwise manage jj workspaces or branches. Later deterministic command steps validate, describe, bookmark, push, and acknowledge.
- Do not use `gh` to post comments. The final command step posts one top-level PR acknowledgement from your summary.
- Read `tmp/execplan-revision-pr.json` for PR metadata.
- Read `tmp/execplan-revision-feedback.md` for normalized GitHub PR feedback. It includes top-level PR comments, review summaries, and inline review comments.
- Read the target plan path printed as `PLAN_PATH=<path>` above.
- Edit only that ExecPlan file. Do not edit source code, tests, config, workflow files, or any other docs.
- If no trusted/actionable feedback requires a plan change, leave the plan unchanged and still write the required summary file.
- Use repository-relative paths only. Do not introduce absolute local paths.

Feedback policy:

- Treat comments from GitHub author associations `OWNER`, `MEMBER`, `COLLABORATOR`, and `CONTRIBUTOR` as trusted/actionable by default.
- Treat comments from other associations as historical context unless the Linear issue explicitly says to act on them.
- Review both inline comments and overall PR/review-thread comments.
- Group related comments mentally by topic and location.
- Consider recency: when comments conflict, prefer the newest applicable feedback and mark older feedback as superseded/no-change in the summary.
- For each actionable feedback item, decide whether it requires:
  - a targeted plan edit,
  - a small amount of repository research before editing, or
  - no document change because it is obsolete, superseded, already addressed, or out of scope.
- If feedback is ambiguous, make the safest targeted plan edit you can and record remaining ambiguity as `[CLARIFY]` in the plan and summary rather than doing open-ended investigation.

Dogfood time budget:

- Use at most 12 tool calls before editing or deciding no edit is needed.
- Inspect only files needed to make the plan revision credible.
- Do not run broad test suites. The validation command handles structural checks.

Revision process:

1. Read `tmp/execplan-revision-pr.json`.
2. Read `tmp/execplan-revision-feedback.md`.
3. Read the ExecPlan at the printed `PLAN_PATH`.
4. Apply focused plan edits for current, trusted, actionable feedback.
5. Preserve the ExecPlan as a self-contained living document. Update `## Decision Log`, `## Risks and Countermeasures`, `## Concrete Steps`, `## Testing and Falsifiability`, or `## Open Questions and Clarifications Needed` when those are the right places for review feedback.
6. Ensure `## Open Questions and Clarifications Needed` remains present; write `None.` only if there are truly no open questions.
7. Write `tmp/execplan-revision-summary.md` with this exact structure:

```markdown
# ExecPlan revision summary

## Outcome
Updated the plan. / No plan changes were needed.

## Feedback addressed
- F001: what changed and where.

## Feedback not changed
- F002: no change because it was superseded/already addressed/out of scope/ambiguous.

## Remaining ambiguity
- None. / [CLARIFY] ...
```

If a section has no entries, write `None.` under that section.

Final response format:

## Summary
One short paragraph stating whether the plan was revised or no changes were needed.

## Plan changes
- Bullet list of the most important changes, or `None`.

## Feedback notes
- Bullet list of superseded/no-change/ambiguous feedback decisions, or `None`.
