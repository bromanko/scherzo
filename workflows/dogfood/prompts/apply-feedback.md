Apply targeted review remediation for Scherzo's `workflow:implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task description:
{{ issue.description }}

Task preparation output:
{{ steps.prepare_context.stdout }}

Implementation summary:
{{ steps.implement.final_response }}

Change analysis output:
{{ steps.analyze_changes.stdout }}

Native review cutover contract output:
{{ steps.assert_native_review_cutover.stdout }}

Native review preparation output:
{{ steps.prepare_review.stdout }}

Native specialist lane structured outputs:

Correctness lane status: {{ steps.lane_correctness.status }}
Correctness structured output status: {{ steps.lane_correctness.structured_output.status }}
Correctness structured output path: {{ steps.lane_correctness.structured_output.path }}
Correctness structured output error: {{ steps.lane_correctness.structured_output.error }}
Correctness evidence verification output:
{{ steps.verify_correctness_evidence.stdout }}
Correctness normalization output:
{{ steps.normalize_correctness.stdout }}

Test-quality lane status: {{ steps.lane_test_quality.status }}
Test-quality structured output status: {{ steps.lane_test_quality.structured_output.status }}
Test-quality structured output path: {{ steps.lane_test_quality.structured_output.path }}
Test-quality structured output error: {{ steps.lane_test_quality.structured_output.error }}
Test-quality evidence verification output:
{{ steps.verify_test_quality_evidence.stdout }}
Test-quality normalization output:
{{ steps.normalize_test_quality.stdout }}

Idioms / maintainability lane status: {{ steps.lane_idioms_maintainability.status }}
Idioms / maintainability structured output status: {{ steps.lane_idioms_maintainability.structured_output.status }}
Idioms / maintainability structured output path: {{ steps.lane_idioms_maintainability.structured_output.path }}
Idioms / maintainability structured output error: {{ steps.lane_idioms_maintainability.structured_output.error }}
Idioms / maintainability evidence verification output:
{{ steps.verify_idioms_maintainability_evidence.stdout }}
Idioms / maintainability normalization output:
{{ steps.normalize_idioms_maintainability.stdout }}

Security / performance lane status: {{ steps.lane_security_performance.status }}
Security / performance structured output status: {{ steps.lane_security_performance.structured_output.status }}
Security / performance structured output path: {{ steps.lane_security_performance.structured_output.path }}
Security / performance structured output error: {{ steps.lane_security_performance.structured_output.error }}
Security / performance evidence verification output:
{{ steps.verify_security_performance_evidence.stdout }}
Security / performance normalization output:
{{ steps.normalize_security_performance.stdout }}

Native synthesis and final review artifact output:
{{ steps.synthesize_review.stdout }}

Native review artifact validation output:
{{ steps.validate_native_review_artifacts.stdout }}

Targeted remediation contract:

- This step replaces the old post-synthesis broad review pass. It is targeted remediation and disposition over synthesized native review findings, not a fresh review of the whole diff.
- You are in the same dedicated workflow workspace prepared by Scherzo as the implementation and native review steps.
- This targeted remediation step is self-contained in the repository. Do not invoke local pi slash commands, home-directory pi skills, or files outside the checkout for remediation behavior.
- Do not create, forget, finish, switch, push, or otherwise manage workflow workspaces.
- Do not create VCS commits, open a PR, or otherwise integrate changes. The publish step uses the configured workspace driver to publish the change after final validation passes.
- Read the final review artifact first. Prefer the `REVIEW_FINAL_ARTIFACT_PATH=...` printed above when present; otherwise read `artifacts/review/synthesize_review/final-review.v1.json` under the run root.
- Iterate over every synthesized finding id in that final artifact. Inspect only the cited files/locations and the relevant changed diff context needed to verify and remediate each finding.
- Fix safe, relevant, medium-or-smaller findings when possible. Do not bluntly defer everything: use bounded judgement to apply small obvious adjacent fixes in touched files when they are directly related to the finding, validation risk, or publishability.
- Avoid a fresh broad review of the whole diff, unrelated cleanup, speculative rewrites, and style-only churn that is not tied to a synthesized finding or obvious nearby validation/publishability risk.
- If no synthesized findings exist, do a short sanity check for obvious workflow breakage and submit an empty `entries` list rather than running a broad review.
- If a finding is invalid, too risky, too broad, or intentionally deferred, explain why in the final response and disposition rationale.
- Submit review finding dispositions by calling `submit_review_finding_dispositions` exactly once as your final structured-output action. Final assistant JSON alone is invalid for this workflow; Scherzo will materialize the validated tool arguments to `tmp/review-finding-dispositions.v1.json` after this step.
- Pass a JSON object with `schema_version` as JSON number `1` (not string `"1"`), `artifact_type` `review_finding_disposition_input`, and one `entries` item per synthesized finding id from `REVIEW_FINAL_ARTIFACT_PATH` / `artifacts/review/synthesize_review/final-review.v1.json`. Cover every synthesized finding id exactly once.
- Each disposition entry must include `finding_id`, `disposition`, `rationale`, and a non-empty `evidence_refs` array of objects. Use `resolved` for fixed findings with diff/test evidence, `rejected` for invalid findings with evidence, `deferred` only for non-blocking findings with a deferral reason, and `obsolete` when later changes make the finding inapplicable.
- Each evidence ref object requires `type` and `description`; it may also include `path`, `ref`, or `command`. Bare string evidence refs are invalid for structured output. Paths/refs must be repository- or run-root-relative with no `..`, absolute paths, or environment placeholders. Examples: `{"type":"path","description":"start-key fix","path":"src/scherzo/workstream/start_key.gleam"}` and `{"type":"command","description":"full validation","command":"direnv exec . gleam test"}`.

Process:

1. Inspect `$SCHERZO_WORKSPACE_DRIVER status --human` and, if needed for cited context, `$SCHERZO_WORKSPACE_DRIVER diff --human`.
2. Read the final review artifact first, then any referenced lane result/log artifacts only as needed for the synthesized findings.
3. For each synthesized finding id, inspect the cited file/location and minimal relevant diff context, then fix safe relevant issues.
4. Run targeted validation if useful and cheap.
5. Call `submit_review_finding_dispositions` with dispositions covering every synthesized finding id exactly once, or an empty `entries` list when the final artifact contains no synthesized findings.
6. Summarize what changed after targeted remediation and mention that structured disposition output was submitted.

Final response format:

## Feedback applied
- Bullet list of fixes made.

## Deferred or rejected feedback
- Bullet list with rationale, or `None`.

## Validation
- Commands you ran, or `Not run; deferred to final workflow validation`.
