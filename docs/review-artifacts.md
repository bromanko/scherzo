# Review artifacts v1

Scherzo's staged code-review workflow passes machine-readable artifacts between review steps. This contract is intentionally separate from the Scherzo daemon runtime: review artifacts are local workflow files for agents and operators to inspect, not durable daemon state and not Linear or GitHub comments.

The aggregate JSON Schema lives at [`.scherzo/workflows/schemas/review-artifacts.v1.schema.json`](../.scherzo/workflows/schemas/review-artifacts.v1.schema.json). Native structured-output review lanes use a two-layer contract: provider-facing tool argument schemas under [`.scherzo/workflows/schemas/provider/`](../.scherzo/workflows/schemas/provider/) accept only model-owned submission fields, then Scherzo materializes those submissions into canonical [`.scherzo/workflows/schemas/review-lane-draft.v1.schema.json`](../.scherzo/workflows/schemas/review-lane-draft.v1.schema.json) artifacts and runs local semantic consistency checks. All retained artifacts use `schema_version: 1` and an `artifact_type` discriminator.

## ExecPlan PR review previews

ExecPlan PR review uses a single checked-in Markdown source artifact: `docs/plans/*.md` by default, or a task-requested repository-relative destination such as `doobar/docs/plans/*.md`. The local helper `scripts/scherzo-execplan-review` may render that Markdown into a temporary HTML viewer under `tmp/scherzo-execplan-review/` so reviewers can use the browser drawer, but that HTML is derived local state, not a checked-in or durable remote artifact. When inline submission is available, comments target the changed Markdown source path and Markdown source line; legacy `docs/plans/*.html` PRs remain supported as legacy source artifacts.

## Artifact types

### `ReviewBrief`

A `ReviewBrief` is the first artifact in the staged review flow. It summarizes the diff or PR and gives later lanes enough shared context to decide what to inspect.

Required fields:

- `schema_version`: `1`.
- `artifact_type`: `review_brief`.
- `generated_at_utc`: timestamp for the brief generation.
- `producer`: tool metadata. The checked-in native preparation producer is `.scherzo/workflows/scripts/scherzo-review`.
- `source`: diff source metadata, including `kind`, `label`, `diff_sha256`, and `changed_file_count`.
- `implementation_summary`: concise summary of the observed change.
- `changed_areas`: per-file subsystem/language/change-kind entries.
- `inferred_acceptance_criteria`: implementation intent or acceptance criteria inferred from the diff and surrounding workflow context.
- `risk_profile`: `level`, `rationale`, and `risk_areas`.
- `suggested_review_lanes`: lane ids and reasons that later review steps can use for routing.
- `test_build_status`: available validation status, or an explicit `unknown` entry when no status was supplied.

The brief is allowed to be heuristic. Review lanes must still inspect the actual diff before making findings.

### `ReviewFinding`

A `ReviewFinding` is the atomic finding object emitted by review lanes. Findings can be standalone artifacts or embedded in a `ReviewLaneResult`.

Required fields:

- `id`: stable within the lane result, for example `security-001`.
- `category`: one of `correctness`, `maintainability`, `security`, `performance`, `testing`, `workflow`, `documentation`, `artifact_contract`, or `other`.
- `severity`: `info`, `low`, `medium`, `high`, or `critical`.
- `evidence_type`: `static`, `test`, `runtime`, `reproduction`, `manual`, `spec`, or `unknown`.
- `verified`: boolean. `true` means the lane validated the finding beyond a hypothesis.
- `blocking`: boolean. `true` means the workflow should not publish until the issue is fixed or explicitly waived.
- `locations`: file/path references with optional line, symbol, diff hunk, or URL data.
- `summary`: short human-readable finding.
- `details`: supporting explanation and evidence.
- `suggested_fix`: actionable remediation.

### `ReviewLaneSubmission`

A `ReviewLaneSubmission` is the provider-facing object passed as the `submit_review_lane_draft` tool arguments by a native review lane. It is not a retained artifact. It deliberately contains only model-owned fields so provider tool schemas stay small and provider-compatible: `draft_findings`, `review_notes`, `evidence_requests`, and `self_check`. The model must not include `schema_version`, `artifact_type`, `generated_at_utc`, `producer`, `lane`, `input_refs`, `remote_mutations`, or `$schema`; Scherzo injects those fields after capture.

Provider schemas live with the workflow bundle at:

- `.scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json`
- `.scherzo/workflows/schemas/provider/review-lane-draft.test-quality.v1.schema.json`
- `.scherzo/workflows/schemas/provider/review-lane-draft.idioms-maintainability.v1.schema.json`
- `.scherzo/workflows/schemas/provider/review-lane-draft.security-performance.v1.schema.json`

These provider schemas are Pi tool parameter schemas only. They intentionally avoid provider-hostile JSON Schema keywords such as `$ref`, `$defs`, `oneOf`, `anyOf`, `allOf`, `enum`, `const`, and union-style `type` arrays. They do not replace canonical artifact validation. The local gate `scripts/scherzo-structured-output-contract check-schema --schema <path>` is the source of truth for this provider-safe subset.

Native lane evidence requests may use only the `evidence_key` values exposed by the provider schema's `evidence_key` description; the executable source of truth is `NATIVE_EVIDENCE_ALLOWLIST` in `.scherzo/workflows/scripts/scherzo_review/review_lane_contract.py`. If a captured submission still proposes another key, Scherzo materializes the retained draft by rewriting that request to `context_only`, clearing its target, and recording a non-fatal normalization diagnostic on the rewritten request before evidence verification.

### `ReviewLaneDraft`

A `ReviewLaneDraft` is the canonical retained artifact produced by Scherzo after it captures a `ReviewLaneSubmission`, rejects runner-owned metadata in the submission, injects deterministic runner metadata, validates the artifact with `.scherzo/workflows/schemas/review-lane-draft.v1.schema.json`, and runs semantic checks such as unique draft finding ids and evidence-request links.

Required fields:

- `schema_version`: `1`.
- `artifact_type`: `review_lane_draft`.
- `generated_at_utc`: timestamp for the draft generation.
- `producer`: agent/tool metadata.
- `lane`: lane metadata (`id`, `name`, `category`, and `version`).
- `input_refs`: local artifact references consumed by the lane.
- `draft_findings`: proposed findings with draft ids, claims, severities, locations, and linked evidence request ids.
- `review_notes`: non-finding notes useful for reviewers or later synthesis.
- `evidence_requests`: requested checks or artifact inspections linked to draft findings.
- `self_check`: agent self-check metadata.
- `remote_mutations`: always `none`.

### `ReviewLaneResult`

A `ReviewLaneResult` records a lane execution and embeds its findings. Lanes may also include `review_notes` for broad risk, coverage, reviewability, or follow-up-test prompts that are useful to reviewers but are not concrete findings.

Required fields:

- `schema_version`: `1`.
- `artifact_type`: `review_lane_result`.
- `lane`: lane metadata (`id`, `name`, `category`, `version`, and optional tool/model data).
- `execution_status`: `state`, start/completion timestamps, and a summary. States are `succeeded`, `failed`, `skipped`, or `blocked`.
- `findings`: list of `ReviewFinding` core objects. An empty list is valid when a lane has no findings.
- `review_notes`: optional list of non-finding notes with `kind`, `category`, `severity`, `locations`, `summary`, `details`, and `suggested_action`.
- `artifacts`: optional local artifact references such as logs, transcripts, or a generated brief.
- `input_brief_ref`: optional reference to the `ReviewBrief` consumed by the lane.

### `ReviewSynthesis`

A `ReviewSynthesis` is produced after the specialist lanes finish. It records lane health, normalized findings, review notes, synthesis actions, grouped findings, and counts. Synthesis responsibilities include:

- deduplicating findings with the same category, summary, and primary location;
- preserving broad risk, coverage, reviewability, and follow-up-test notes separately from concrete findings;
- downgrading correctness blockers that lack verified executable evidence (`test`, `runtime`, or `reproduction`);
- recording conflicting remediation advice as alternatives instead of emitting duplicate comments;
- grouping findings by severity and category; and
- carrying failed lane state forward as `execution_issues` without treating review findings themselves as workflow failures.

### `FinalReviewArtifact`

A `FinalReviewArtifact` is the concise human-facing review artifact generated from the synthesis. It always exists when synthesis receives valid lane result artifacts, including when all lanes return empty findings. It includes the final Markdown review body, grouped findings, blocker evidence, non-blocking findings, risk/coverage/review notes, lane statuses, and `remote_mutations: "none"` for local-artifact safety.

## Native preparation entrypoint

Use `.scherzo/workflows/scripts/scherzo-review prepare-native` to generate the shared inputs consumed by native Scherzo review lanes. The command is local-only: it reads a diff source, writes retained artifacts, and does not post comments, update Linear, push branches, check out PRs, or mutate remote state.

Examples:

```sh
# Current jj change, using @-..@ by default.
.scherzo/workflows/scripts/scherzo-review prepare-native --output-dir tmp/scherzo-review-native

# Explicit local jj range.
.scherzo/workflows/scripts/scherzo-review prepare-native --from main@origin --to @ --output-dir tmp/scherzo-review-native

# Saved unified diff.
.scherzo/workflows/scripts/scherzo-review prepare-native --diff-file /path/to/change.diff --output-dir tmp/scherzo-review-native

# GitHub PR diff. This uses read-only gh pr diff/view calls only.
.scherzo/workflows/scripts/scherzo-review prepare-native --pr 74 --repo scherzo-systems/scherzo --output-dir tmp/scherzo-review-native
```

The command writes:

- `review-brief.v1.json`: `ReviewBrief` artifact for prompt-facing summary and routing context.
- `diff.patch`: exact unified diff consumed by downstream lanes.
- `source-metadata.v1.json`: source kind, label, diff checksum, and changed-file count.
- `changed-files.v1.json`: per-file language, subsystem, hunk, and sample-line metadata.
- `validation-status.v1.json`: normalized pre-native validation evidence.
- `context-manifest.v1.json`: bounded context snapshot manifest with per-file availability and truncation metadata.
- `manifest.v1.json`: local checksums for quick inspection.
- `review-native-prepare.log`: bounded local execution log.

It prints the artifact paths as `REVIEW_BRIEF_PATH=...`, `REVIEW_DIFF_PATH=...`, `REVIEW_CHANGED_FILES_PATH=...`, `REVIEW_VALIDATION_STATUS_PATH=...`, and `REVIEW_CONTEXT_MANIFEST_PATH=...`. It also prints `REVIEW_REMOTE_MUTATIONS=none` as an explicit safety marker.

For native review preparation, `validation-status.v1.json` is the explicit pre-native validation artifact. It keeps `schema_version: 1` and `artifact_type: "validation_status"`, adds an `overall_state`, and carries compatibility arrays under both `status` and `test_build_status`. Each entry may include `name`, compatibility `status`, precise `state`, `source`, `summary`, `command`, optional integer `exit_status`, bounded `output_excerpt`, and repository- or run-root-relative `artifact_refs`. Recognized sources currently include `structured_validation_artifact`, `not_yet_run_by_design`, `validation_artifact_missing`, `malformed_validation_artifact`, and `cli`. Full command output stays in retained command-step diagnostics rather than prompt-facing artifacts.

Validate prepared artifacts with:

```sh
.scherzo/workflows/scripts/scherzo-review validate --artifact tmp/scherzo-review-native/review-brief.v1.json
.scherzo/workflows/scripts/scherzo-review validate --artifact tmp/scherzo-review-native/validation-status.v1.json
.scherzo/workflows/scripts/scherzo-review validate --artifact tmp/scherzo-review-native/changed-files.v1.json
.scherzo/workflows/scripts/scherzo-review validate --artifact tmp/scherzo-review-native/context-manifest.v1.json
```

The validator is deliberately minimal and dependency-free. The JSON Schema remains the documentation source of truth for future, richer validators.

## Native lane evidence and normalization entrypoints

Normal implementation and execplan-implementation review runs use native Scherzo `kind: agent` lane steps with `submit_review_lane_draft` structured-output tool submissions. Scherzo captures those submissions, rejects runner-owned metadata supplied by the model, injects deterministic lane/input metadata, validates the canonical `ReviewLaneDraft`, and then runs local evidence verification and lane-result normalization.

The supported native lane ids are:

- `correctness`: looks for behavior and logic bugs. Blocking correctness findings must be verified with executable evidence (`test`, `runtime`, or `reproduction`); static-only concerns are emitted as non-blocking suspicions.
- `test-quality`: checks whether implementation changes have meaningful committed tests, flags shallow or assertion-light test changes, and includes concrete proposed test cases for coverage gaps.
- `idioms-maintainability`: separates `must-fix`, `should-fix`, and `optional/nit` feedback for clarity, structure, production fatal constructs, and reviewability.
- `security-performance`: chooses lightweight or deeper review based on the `ReviewBrief.risk_profile`, inspecting high-risk boundaries more carefully.

Evidence verification consumes a canonical draft plus the prepared native inputs and writes `evidence-ledger.v1.json`:

```sh
.scherzo/workflows/scripts/scherzo-review verify-evidence \
  --lane correctness \
  --draft tmp/native-lanes/correctness/review-lane-draft.v1.json \
  --brief tmp/scherzo-review-native/review-brief.v1.json \
  --diff-file tmp/scherzo-review-native/diff.patch \
  --changed-files tmp/scherzo-review-native/changed-files.v1.json \
  --validation-status tmp/scherzo-review-native/validation-status.v1.json \
  --context-manifest tmp/scherzo-review-native/context-manifest.v1.json \
  --output-dir tmp/native-lanes/correctness/evidence
```

Lane normalization consumes the draft and evidence ledger and writes `review-lane-<lane-id>.v1.json`:

```sh
.scherzo/workflows/scripts/scherzo-review normalize-lane-result \
  --lane correctness \
  --draft tmp/native-lanes/correctness/review-lane-draft.v1.json \
  --evidence-ledger tmp/native-lanes/correctness/evidence/evidence-ledger.v1.json \
  --brief tmp/scherzo-review-native/review-brief.v1.json \
  --output-dir tmp/native-lanes/correctness/result
```

If a native lane fails before producing a usable draft, normalization still attempts to write `review-lane-<lane-id>.v1.json` with `execution_status.state: "failed"` and retained diagnostics, so malformed drafts, structured-output validation failures, timeouts, and containment failures remain debuggable from retained workflow artifacts. Static-only correctness claims are downgraded into `review_notes` unless a blocking correctness finding references verified executable evidence from `evidence-ledger.v1.json`.

## Synthesis and final artifact entrypoint

Use `.scherzo/workflows/scripts/scherzo-review synthesize` after the specialist lanes:

```sh
.scherzo/workflows/scripts/scherzo-review synthesize \
  --brief tmp/scherzo-review-native/review-brief.v1.json \
  --lane-result tmp/native-lanes/correctness/result/review-lane-correctness.v1.json \
  --lane-result tmp/native-lanes/test-quality/result/review-lane-test-quality.v1.json \
  --lane-result tmp/native-lanes/idioms/result/review-lane-idioms-maintainability.v1.json \
  --lane-result tmp/native-lanes/security/result/review-lane-security-performance.v1.json \
  --output-dir tmp/review-synthesis
```

The command writes:

- `review-synthesis.v1.json`: normalized, deduplicated lane synthesis with lane health and synthesis actions.
- `final-review.v1.json`: concise final review artifact with Markdown output and grouped findings.
- `review-synthesis.log`: local execution log.
- `manifest.v1.json`: checksums for the synthesis artifacts.

It prints `REVIEW_SYNTHESIS_PATH=...`, `REVIEW_FINAL_ARTIFACT_PATH=...`, `REVIEW_LANE_FAILURES=...`, and `REVIEW_REMOTE_MUTATIONS=none`. Failed lanes represented by valid `ReviewLaneResult` artifacts are isolated into `execution_issues`; malformed or missing lane artifacts are infrastructure errors and cause the command to fail.

## Review-lane contract validation

The local contract command is the test harness for review-lane schema, prompt, and provider-compatibility changes. The required offline check does not create Linear runs, prepare jj workspaces, push branches, or mutate remote state:

```sh
.scherzo/workflows/scripts/scherzo-review-lane-contract offline \
  --workflow .scherzo/workflows/implementation.yaml \
  --fixtures test/fixtures/review-lane-contract \
  --output-dir tmp/scherzo-review-lane-contract/offline/implementation

.scherzo/workflows/scripts/scherzo-review-lane-contract offline \
  --workflow .scherzo/workflows/execplan-implementation.yaml \
  --fixtures test/fixtures/review-lane-contract \
  --output-dir tmp/scherzo-review-lane-contract/offline/execplan-implementation
```

It first delegates generic source-policy, provider-schema, prompt/tool, and provider/downstream-alignment checks to `scripts/scherzo-structured-output-contract check-workflow`, then verifies the routed implementation review workflow wiring, runs all lane fixtures, materializes valid submissions into canonical drafts, runs canonical JSON Schema and semantic validation, and writes `contract-report.v1.json` with `remote_mutations: "none"`.

Operators can check one schema or one captured submission directly:

```sh
.scherzo/workflows/scripts/scherzo-review-lane-contract check-schema \
  --schema .scherzo/workflows/schemas/provider/review-lane-draft.correctness.v1.schema.json

.scherzo/workflows/scripts/scherzo-review-lane-contract materialize \
  --lane correctness \
  --submission test/fixtures/review-lane-contract/correctness/valid-minimal.arguments.json \
  --prepare-dir test/fixtures/review-lane-contract/prepared-review \
  --output tmp/scherzo-review-lane-contract/correctness/review-lane-draft.v1.json
```

The optional live-provider canary is separate from the required `scripts/scherzo-ci` gate because it may need provider credentials or incur provider cost:

```sh
.scherzo/workflows/scripts/scherzo-review-lane-contract live \
  --workflow .scherzo/workflows/implementation.yaml \
  --output-dir tmp/scherzo-review-lane-contract/live \
  --skip-if-missing-credentials
```

With no credentials this command writes a skipped report with `skipped_missing_credentials`. Hosted CI should enable provider-backed checks only after confirming credential and cost policy.

When `SCHERZO_REVIEW_LANE_PREFLIGHT_MODE=required-live`, dispatcher preflight uses the same provider-backed live probe before claiming an implementation issue. It caches matching results in `<workspace-root>/.scherzo-state/review-lane-contract-cache.v1.json`; entries include the workflow fingerprint, provider/model identity, review-lane tool names, provider schema digests, checker version, mode, status, blocking flag, `checked_at_ms`, and `expires_at_ms`. Deleting the cache file is safe and forces a fresh preflight. Setting `SCHERZO_REVIEW_LANE_PREFLIGHT_CACHE_TTL_SECONDS=0` disables cache reuse.

## Checked-in workflow integration

The dogfood `implementation` and `execplan-implementation` workflows generate the brief immediately before review and store review artifacts under the run artifact directory:

```text
$SCHERZO_RUN_ROOT/artifacts/review/<step-id>/
```

After native preparation, the dogfood implementation workflows run the four specialist lanes as Scherzo-managed `kind: agent` steps with required structured output sourced from the generic `submit_review_lane_draft` Pi tool call registered through `.pi/extensions/scherzo-structured-output`. There is no longer a separate review-lane rollback extension shim. Lane draft artifacts are retained by Scherzo under the run artifact store, then `.scherzo/workflows/scripts/scherzo-review verify-evidence`, `normalize-lane-result`, and `synthesize` produce `ReviewLaneResult`, `ReviewSynthesis`, and `FinalReviewArtifact` files under `$SCHERZO_RUN_ROOT/artifacts/review/`. Required JSON outputs retry validation failures once by default (`structured_output.validation_retries`, set to `0` to disable; values above `1` are rejected to keep recovery bounded) with a compact retry prompt that references retained run artifacts instead of replaying diffs, transcripts, or prior full responses. Native review lane `agent_pi_failed` failures caused by `stopReason=error: terminated` are treated as transient and spend the same single retry budget; other agent failures remain lane failures with retained step diagnostics. Workspace mutation checks fail closed before evidence verification. Malformed lane output and evidence-verification failures normalize into failed lane results with retained diagnostics; the native review validation gate blocks publication if the final artifact reports lane failures or execution issues. The checked-in implementation workflows then run a single targeted remediation/disposition agent (`apply_feedback` or `apply_review_feedback`) directly after native artifact validation. That agent reads the final review artifact first, fixes only safe synthesized findings and obvious nearby validation/publishability risks, and submits `review_finding_disposition_input`; it is intentionally not a second broad `code_review` / `review_changes` pass.

The brief, lane, synthesis, and final review steps write local artifacts only; they do not post PR comments, update Linear, push, rebase, check out PR branches, or alter PR state.

The checked-in dogfood workflows allow the four native review lanes to run concurrently after the clean-workspace checkpoint, bounded by workflow-level `concurrency`. Operators who hit provider quota, rate-limit, or local resource pressure can lower `concurrency` in the workflow YAML to serialize or partially throttle lane execution without changing the lane artifact contract.

## Compatibility and versioning

- Add fields freely when consumers can ignore unknown keys.
- Do not change the meaning or type of existing v1 fields.
- Introduce `schema_version: 2` for breaking changes and keep the v1 schema available for old artifacts.
- Store artifacts in workflow-local output directories or another inspectable local artifact store. Do not rely on Linear comments or PR review comments as the source of truth for the contract.
