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

Synthesis and final review artifact output:
{{ steps.synthesize_review.stdout }}

Review contract:

- You are in the same dedicated workflow workspace prepared by Scherzo as the implementation.
- Do not create, forget, finish, switch, push, or otherwise manage workflow workspaces.
- Use `$SCHERZO_WORKSPACE_DRIVER status --human` and `$SCHERZO_WORKSPACE_DRIVER diff --human` only for orientation; the analysis output above is authoritative for changed files across the workflow run.
- If `REVIEW_BRIEF_PATH=...` is present in the dry-run output, read that local artifact for orientation. It is additive context only; do not post the artifact to PRs or Linear.
- If any specialist lane output includes `REVIEW_LANE_RESULT_PATH=...`, read the referenced `ReviewLaneResult` and its log/analysis artifacts produced by `scripts/scherzo-review`. Treat lane findings as normalized review input: fix or report blocking findings, preserve non-blocking suspicions as feedback, and do not discard empty-finding lane logs.
- If synthesis output includes `REVIEW_SYNTHESIS_PATH=...` or `REVIEW_FINAL_ARTIFACT_PATH=...`, read the referenced artifacts first. Use the final artifact as the concise normalized review input, including lane failures and downgraded/unproven correctness claims, but still inspect the actual diff before applying fixes.
- This review step is self-contained in the repository. Do not invoke local pi slash commands, home-directory pi skills, or files outside the checkout for review behavior.
- Use the staged review artifacts produced by `scripts/scherzo-review` as the normalized specialist review input. If `REVIEW_FINAL_ARTIFACT_PATH=...` is present, treat that final artifact as the primary findings list, including lane failures, blocking findings, downgraded findings, and retained notes.
- Inspect the actual diff before applying a fix. The staged artifacts guide review scope, but the current files decide whether a finding is still valid and safe to fix.
- If a staged lane failed or a synthesis artifact is missing, use the available lane logs, the change analysis output, and a bounded manual inspection of changed files. Do not fall back to language-specific local pi skills.
- If `LANGUAGES=none`, do not invent a language review. Check the changed files briefly for obvious workflow breakage and report that no supported language review was required.
- Treat unsupported review-relevant files listed in `UNSUPPORTED_REVIEW_FILES` as out of scope unless they are obviously broken by the current change.
- Keep changes focused. Do not start unrelated cleanup.
- Do not run final full validation; the workflow has a dedicated final validation step.

Review process:

1. Read the change analysis output and staged review command outputs.
2. Read the final review artifact and any referenced lane result/log artifacts.
3. Inspect the actual diff and current files only as needed to verify findings.
4. Apply safe and relevant medium-or-smaller fixes identified by the staged artifacts or bounded inspection.
5. Leave risky, broad, ambiguous, or unsupported findings for the feedback step with a clear explanation.
6. Finish with a concise review report.

Final response format:

## Review performed
- Staged review artifacts and files inspected, or `No supported language review required`.

## Fixes applied
- Bullet list of safe fixes made, or `None`.

## Remaining findings
- Bullet list of findings for the feedback step, or `None`.

## Notes
Anything the final feedback or validation step should know.
