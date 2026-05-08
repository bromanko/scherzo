You are reviewing the implementation produced by Scherzo's `workflow:implementation` workflow for Linear issue {{ issue.identifier }}: {{ issue.title }}.

Issue description:
{{ issue.description }}

Ticket preparation output:
{{ steps.prepare_context.stdout }}

Implementation step response:
{{ steps.implement.final_response }}

Change analysis output:
{{ steps.analyze_changes.stdout }}

Review brief dry-run output:
{{ steps.generate_review_brief.stdout }}

Specialist review lane outputs:

Correctness lane:
{{ steps.correctness_review_lane.stdout }}

Test-quality lane:
{{ steps.test_quality_review_lane.stdout }}

Idioms / maintainability lane:
{{ steps.idioms_maintainability_review_lane.stdout }}

Security / performance lane:
{{ steps.security_performance_review_lane.stdout }}

Review contract:

- You are in the same dedicated jj workspace as the implementation.
- Do not create, forget, finish, switch, push, or otherwise manage jj workspaces.
- Use `jj status --color=never` and `jj diff --from @- --to @ --color=never` only for orientation; the analysis output above is authoritative for changed files across the workflow run.
- If `REVIEW_BRIEF_PATH=...` is present in the dry-run output, read that local artifact for orientation. It is additive context only; preserve the existing review behavior and do not post the artifact to PRs or Linear.
- If any specialist lane output includes `REVIEW_LANE_RESULT_PATH=...`, read the referenced `ReviewLaneResult` and its log/analysis artifacts before running the existing review. Treat lane findings as normalized review input: fix or report blocking findings, preserve non-blocking suspicions as feedback, and do not discard empty-finding lane logs.
- The workflow currently supports project-local Gleam review only. If `LANGUAGES=gleam`, use the vendored project-local review skill content under `.pi/skills/` and run the equivalent of `/review gleam --fix medium`.
- If the local `/review` command is unavailable or does not accept `--fix medium`, read `.pi/skills/gleam-review/SKILL.md` and the related `.pi/skills/gleam-*-review/SKILL.md` files, perform the Gleam review manually against the changed files, and apply only safe medium-or-smaller fixes.
- If `LANGUAGES=none`, do not invent a language review. Check the changed files briefly for obvious workflow breakage and report that no supported language review was required.
- Treat unsupported review-relevant files listed in `UNSUPPORTED_REVIEW_FILES` as out of scope unless they are obviously broken by the current change.
- Keep changes focused. Do not start unrelated cleanup.
- Do not run final full validation; the workflow has a dedicated final validation step.

Review process:

1. Read the change analysis output and identify the review commands to run.
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
