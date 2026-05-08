You are reviewing the implementation produced by Scherzo's `workflow:execplan-implementation` workflow for Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue description:
{{ issue.description }}

Implementation step response:
{{ steps.implement_plan.final_response }}

Post-plan-feedback change analysis output:
{{ steps.analyze_changes_after_plan_feedback.stdout }}

Plan-completion gate output:
{{ steps.gate_plan_completion.stdout }}

Review brief dry-run output:
{{ steps.generate_review_brief.stdout }}

Review contract:

- You are in the same dedicated jj workspace as the implementation.
- Do not create, forget, finish, switch, push, or otherwise manage jj workspaces.
- Use `jj status --color=never` and `jj diff --from @- --to @ --color=never` only for orientation; the analysis output above is authoritative for changed files across the workflow run.
- If `REVIEW_BRIEF_PATH=...` is present in the dry-run output, read that local artifact for orientation. It is additive context only; preserve the existing review behavior and do not post the artifact to PRs or Linear.
- The workflow currently supports project-local Gleam review only. If `LANGUAGES=gleam`, use the vendored project-local review skill content under `.pi/skills/` and run the equivalent of `/review gleam --fix medium`.
- If the local `/review` command is unavailable or does not accept `--fix medium`, read `.pi/skills/gleam-review/SKILL.md` and the related `.pi/skills/gleam-*-review/SKILL.md` files, perform the Gleam review manually against the changed files, and apply only safe medium-or-smaller fixes.
- If `LANGUAGES=none`, do not invent a language review. Check the changed files briefly for obvious workflow breakage and report that no supported language review was required.
- Treat unsupported review-relevant files listed in `UNSUPPORTED_REVIEW_FILES` as out of scope for this first version unless they are obviously broken by the current change.
- Keep changes focused. Do not start unrelated cleanup.
- Do not run final full validation; the workflow has a dedicated final validation step.

Review process:

1. Read the post-plan-feedback change analysis output and identify the review commands to run.
2. Run the supported project-local review(s), currently Gleam.
3. Apply safe and relevant medium-or-smaller fixes if the review tooling or your manual review identifies them.
4. Leave risky, broad, or ambiguous findings for the feedback step with a clear explanation.
5. Finish with a concise review report.

Final response format:

## Review performed
- Review commands run, or `No supported language review required`.

## Fixes applied
- Bullet list of safe fixes made, or `None`.

## Remaining findings
- Bullet list of findings for the feedback step, or `None`.

## Notes
Anything the final feedback or validation step should know.
