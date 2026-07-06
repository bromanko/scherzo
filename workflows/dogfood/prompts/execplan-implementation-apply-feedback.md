Apply targeted review remediation for Scherzo's `workflow:execplan-implementation` workflow on task {{ issue.identifier }}: {{ issue.title }}.

Task URL:
{{ issue.url }}

ExecPlan identity model:

{% include "fragments/execplan-identity-model.md" %}
- Do not report a conflict, fail completion, or request revision solely because the handoff issue, source issue, review doc path, or implementation pack provenance reference different Linear keys. Treat only review-doc/implementation-pack disagreement in intent, scope, acceptance, safety, or source-plan provenance beyond that expected split as blocking.

Bundle preparation output:
{{ steps.prepare_bundle.stdout }}

Implementation summary:
{{ steps.implement_plan.final_response }}

Change analysis output:
{{ steps.analyze_changes.stdout }}

Initial plan-completion verifier response:
{{ steps.verify_plan_completion.final_response }}

Validation before native review output:
{{ steps.validate_before_native_review.stdout }}

Native review preparation output:
{{ steps.prepare_review.stdout }}

Native specialist lane structured outputs:

Correctness lane status: {{ steps.lane_correctness.status }}
Correctness structured output status: {{ steps.lane_correctness.structured_output.status }}
Correctness structured output path: {{ steps.lane_correctness.structured_output.path }}
Correctness structured output error: {{ steps.lane_correctness.structured_output.error }}

Test-quality lane status: {{ steps.lane_test_quality.status }}
Test-quality structured output status: {{ steps.lane_test_quality.structured_output.status }}
Test-quality structured output path: {{ steps.lane_test_quality.structured_output.path }}
Test-quality structured output error: {{ steps.lane_test_quality.structured_output.error }}

Idioms / maintainability lane status: {{ steps.lane_idioms_maintainability.status }}
Idioms / maintainability structured output status: {{ steps.lane_idioms_maintainability.structured_output.status }}
Idioms / maintainability structured output path: {{ steps.lane_idioms_maintainability.structured_output.path }}
Idioms / maintainability structured output error: {{ steps.lane_idioms_maintainability.structured_output.error }}

Security / performance lane status: {{ steps.lane_security_performance.status }}
Security / performance structured output status: {{ steps.lane_security_performance.structured_output.status }}
Security / performance structured output path: {{ steps.lane_security_performance.structured_output.path }}
Security / performance structured output error: {{ steps.lane_security_performance.structured_output.error }}

Native lane finalization, synthesis, evidence verification, and final review artifact output:
{{ steps.finalize_lanes.stdout }}


Targeted remediation contract:

- This step replaces the old post-synthesis broad review pass. It is targeted remediation and disposition over synthesized native review findings, not a fresh review of the whole diff.
- You are in the same dedicated workflow workspace prepared by Scherzo as the implementation and native review steps.
- This targeted remediation step is self-contained in the repository. Do not invoke local pi slash commands, home-directory pi skills, or files outside the checkout for remediation behavior.
- Do not create, forget, finish, switch, push, or otherwise manage workflow workspaces.
- Do not create VCS commits. The publish step uses the configured workspace driver to publish the final change after final validation passes.
- Read the final review artifact first. Prefer the `REVIEW_FINAL_ARTIFACT_PATH=...` printed above when present; otherwise read `artifacts/review/synthesize_review/final-review.v1.json` under the run root.
- Iterate over every synthesized finding id in that final artifact. Inspect only the cited files/locations and the relevant changed diff context needed to verify and remediate each finding.
- Fix safe, relevant, medium-or-smaller findings when possible. Do not bluntly defer everything: use bounded judgement to apply small obvious adjacent fixes in touched files when they are directly related to the finding, validation risk, or publishability.
- Avoid a fresh broad review of the whole diff, unrelated cleanup, speculative rewrites, and style-only churn that is not tied to a synthesized finding or obvious nearby validation/publishability risk.
- If no synthesized findings exist, do a short sanity check for obvious workflow breakage and submit an empty `entries` list rather than running a broad review.
- If a finding is invalid, too risky, too broad, or intentionally deferred, explain why in the final response and disposition rationale. Do not convert explicitly deferred post-implementation manual verification into a blocking review fix; preserve it for handoff.
- Submit review finding dispositions by calling `submit_review_finding_dispositions` exactly once as your final structured-output action. Final assistant JSON alone is invalid for this workflow; Scherzo will materialize the validated tool arguments to `tmp/review-finding-dispositions.v1.json` after this step.
- Pass a JSON object with `schema_version` as JSON number `1` (not string `"1"`), `artifact_type` `review_finding_disposition_input`, and one `entries` item per synthesized finding id from `REVIEW_FINAL_ARTIFACT_PATH` / `artifacts/review/synthesize_review/final-review.v1.json`. Cover every synthesized finding id exactly once.
- Each disposition entry must include `finding_id`, `disposition`, `rationale`, and a non-empty `evidence_refs` array of objects. Use `resolved` for fixed findings with diff/test evidence, `rejected` for invalid findings with evidence, `deferred` only for non-blocking findings with a deferral reason, and `obsolete` when later changes make the finding inapplicable.
- Each evidence ref object requires `type` and `description`; it may also include `path`, `ref`, or `command`. Bare string evidence refs are invalid for structured output. Paths/refs must be repository- or run-root-relative with no `..`, absolute paths, or environment placeholders. Examples: `{"type":"path","description":"start-key fix","path":"src/scherzo/workstream/start_key.gleam"}` and `{"type":"command","description":"full validation","command":"direnv exec . gleam test"}`.
- Read `$SCHERZO_RUN_ROOT/state/implementation/metadata.json`, `$SCHERZO_RUN_ROOT/state/implementation/execplan-bundle.json`, `$SCHERZO_RUN_ROOT/state/implementation/execplan-review-doc.md`, and `$SCHERZO_RUN_ROOT/state/implementation/execplan-implementation-pack.json` when plan context is needed.
- Treat `$SCHERZO_RUN_ROOT/state/implementation/execplan-review-doc.md` as the authoritative canonical plan prepared from the descriptor `plan` entry in `exec_plan_bundle.entries`; metadata `plan_path` points at that local copy.
- Do not edit the prepared canonical plan artifact after review; report any living-document updates that should happen through a follow-up ExecPlan revision or optional review surface.
- Do not try to refresh `$SCHERZO_RUN_ROOT/state/implementation/scherzo-plan-completion-verdict.json` yourself; the workflow runs a final plan-completion verifier before final validation so any tracked review fixes are checked before publish.

Process:

1. Inspect `$SCHERZO_WORKSPACE_DRIVER status --human` and, if needed for cited context, `$SCHERZO_WORKSPACE_DRIVER diff --human`.
2. Read the final review artifact first, then any referenced lane result/log artifacts only as needed for the synthesized findings.
3. For each synthesized finding id, inspect the cited file/location and minimal relevant diff context, then fix safe relevant issues.
4. Read the ExecPlan context files listed above only as needed to understand a finding or avoid conflicting with the canonical plan.
5. Run targeted validation if useful and cheap.
6. Call `submit_review_finding_dispositions` with dispositions covering every synthesized finding id exactly once, or an empty `entries` list when the final artifact contains no synthesized findings.
7. Report any canonical-plan/review-surface living-document updates needed because of your changes.
8. Summarize what changed after targeted remediation and mention that structured disposition output was submitted.

Final response format:

## Feedback applied
- Bullet list of fixes made.

## Deferred or rejected feedback
- Bullet list with rationale, or `None`.

## Validation
- Commands you ran, or `Not run; deferred to final workflow validation`.
