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
- `producer`: tool metadata. The checked-in dry-run producer is `.scherzo/workflows/scripts/scherzo-review`.
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

A `FinalReviewArtifact` is the concise human-facing review artifact generated from the synthesis. It always exists when synthesis receives valid lane result artifacts, including when all lanes return empty findings. It includes the final Markdown review body, grouped findings, blocker evidence, non-blocking findings, risk/coverage/review notes, lane statuses, and `remote_mutations: "none"` for dry-run/preflight safety.

## Dry-run entrypoint

Use `.scherzo/workflows/scripts/scherzo-review dry-run` to generate a schema-valid brief without posting comments, updating Linear, pushing branches, checking out a PR, or mutating remote state.

Examples:

```sh
# Current jj change, using @-..@ by default.
.scherzo/workflows/scripts/scherzo-review dry-run --output-dir tmp/review-dry-run

# Explicit local jj range.
.scherzo/workflows/scripts/scherzo-review dry-run --from main@origin --to @ --output-dir tmp/review-dry-run

# Saved unified diff.
.scherzo/workflows/scripts/scherzo-review dry-run --diff-file /path/to/pr.diff --output-dir tmp/review-dry-run

# GitHub PR diff. This uses read-only gh pr diff/view calls only.
.scherzo/workflows/scripts/scherzo-review dry-run --pr 74 --repo scherzo-systems/scherzo --output-dir tmp/review-dry-run
```

The command writes:

- `review-brief.v1.json`: `ReviewBrief` artifact.
- `review-lane-result.v1.json`: `ReviewLaneResult` for the brief-generation lane.
- `manifest.v1.json`: local checksums for quick inspection.
- `review-dry-run.log`: bounded local execution log.

It prints the artifact paths as `REVIEW_BRIEF_PATH=...`, `REVIEW_LANE_RESULT_PATH=...`, and `REVIEW_LOG_PATH=...`. It also prints `REVIEW_REMOTE_MUTATIONS=none` as an explicit safety marker.

Validate an artifact with:

```sh
.scherzo/workflows/scripts/scherzo-review validate --artifact tmp/review-dry-run/review-brief.v1.json
.scherzo/workflows/scripts/scherzo-review validate --artifact tmp/review-dry-run/review-lane-result.v1.json
```

The validator is deliberately minimal and dependency-free. The JSON Schema remains the documentation source of truth for future, richer validators.

## Manual legacy specialist lane entrypoint

`.scherzo/workflows/scripts/scherzo-review run-lane` remains available for local/manual artifact validation and historical fixture coverage only. It is not the production staged-review path for implementation workflows, and operators must not use `SCHERZO_STAGED_REVIEW_AGENT_BACKEND` or `--agent-backend heuristic|fixture|external` to route normal implementation review. Normal implementation and execplan-implementation runs use native Scherzo `kind: agent` lane steps with `submit_review_lane_draft` structured-output tool submissions.

When intentionally validating the legacy helper, run one specialist lane against a diff source plus an existing `ReviewBrief` and choose an explicit backend. The `heuristic` backend preserves the deterministic first-version lane behavior, `fixture` exercises deterministic local fixtures, and `external` exercises the legacy external command contract when `SCHERZO_REVIEW_AGENT_COMMAND` is configured.

```sh
.scherzo/workflows/scripts/scherzo-review run-lane \
  --lane correctness \
  --brief tmp/review-dry-run/review-brief.v1.json \
  --diff-file /path/to/pr.diff \
  --output-dir tmp/review-lanes/correctness \
  --agent-backend fixture
```

The supported lane ids are:

- `correctness`: looks for behavior and logic bugs. Blocking correctness findings must be verified with executable evidence (`test`, `runtime`, or `reproduction`); static-only concerns are emitted as non-blocking suspicions.
- `test-quality`: checks whether implementation changes have meaningful committed tests, flags shallow or assertion-light test changes, and includes concrete proposed test cases for coverage gaps.
- `idioms-maintainability`: separates `must-fix`, `should-fix`, and `optional/nit` feedback for clarity, structure, production fatal constructs, and reviewability.
- `security-performance`: chooses `lightweight`, `standard`, or `deep` review depth from the `ReviewBrief.risk_profile`, staying lightweight for low-risk diffs and inspecting high-risk boundaries more deeply.

Each successful lane writes:

- `review-lane-<lane-id>.v1.json`: a schema-valid `ReviewLaneResult`.
- `review-lane-<lane-id>-analysis.v1.json`: local diagnostic details about checks performed, selected depth, risk profile, changed files, finding counts, and empty-finding rationale.
- `review-lane-<lane-id>.log`: a bounded execution log with source, brief checksum, diff checksum, checks, and empty-finding reason when applicable.

Agent-backed lane runs also retain an input bundle and backend evidence:

- `input/review-brief.v1.json`, `input/diff.patch`, `input/changed-files.v1.json`, `input/validation-status.v1.json`, and `input/context-manifest.v1.json`.
- `prompt.md`, `raw-agent-output.json`, and transcript files when an external backend runs.
- `evidence-ledger.v1.json` plus reproduction stdout/stderr/command logs when the harness runs trusted executable evidence.

For native review preparation, `validation-status.v1.json` is the explicit pre-native validation artifact. It keeps `schema_version: 1` and `artifact_type: "validation_status"`, adds an `overall_state`, and carries compatibility arrays under both `status` and `test_build_status`. Each entry may include `name`, compatibility `status`, precise `state`, `source`, `summary`, `command`, optional integer `exit_status`, bounded `output_excerpt`, and repository- or run-root-relative `artifact_refs`. Recognized sources currently include `structured_validation_artifact`, `not_yet_run_by_design`, `validation_artifact_missing`, `malformed_validation_artifact`, and `cli`. Full command output stays in retained command-step diagnostics rather than prompt-facing artifacts.

For agent-backed correctness lanes, a blocking correctness finding must reference a harness-issued evidence id from `evidence-ledger.v1.json` with executable evidence type `test`, `runtime`, or `reproduction`. Static-only correctness claims are downgraded into `review_notes`.

If a lane fails before producing findings, it still attempts to write `review-lane-<lane-id>.v1.json` with `execution_status.state: "failed"` and a log artifact containing the error, so malformed briefs, missing external backend configuration, timeouts, malformed raw output, and containment failures are debuggable from retained workflow artifacts.

The command prints `REVIEW_LANE_RESULT_PATH=...`, `REVIEW_LANE_LOG_PATH=...`, and, on success, `REVIEW_LANE_ANALYSIS_PATH=...` for workflow steps and tests to consume. It is local-only and does not post PR comments, update Linear, push branches, or mutate remote state.

## Synthesis and final artifact entrypoint

Use `.scherzo/workflows/scripts/scherzo-review synthesize` after the specialist lanes:

```sh
.scherzo/workflows/scripts/scherzo-review synthesize \
  --brief tmp/review-dry-run/review-brief.v1.json \
  --lane-result tmp/review-lanes/correctness/review-lane-correctness.v1.json \
  --lane-result tmp/review-lanes/test-quality/review-lane-test-quality.v1.json \
  --lane-result tmp/review-lanes/idioms/review-lane-idioms-maintainability.v1.json \
  --lane-result tmp/review-lanes/security/review-lane-security-performance.v1.json \
  --output-dir tmp/review-synthesis
```

The command writes:

- `review-synthesis.v1.json`: normalized, deduplicated lane synthesis with lane health and synthesis actions.
- `final-review.v1.json`: concise final review artifact with Markdown output and grouped findings.
- `review-synthesis.log`: local execution log.
- `manifest.v1.json`: checksums for the synthesis artifacts.

It prints `REVIEW_SYNTHESIS_PATH=...`, `REVIEW_FINAL_ARTIFACT_PATH=...`, `REVIEW_LANE_FAILURES=...`, and `REVIEW_REMOTE_MUTATIONS=none`. Failed lanes represented by valid `ReviewLaneResult` artifacts are isolated into `execution_issues`; malformed or missing lane artifacts are infrastructure errors and cause the command to fail.

## E2E preflight entrypoint

A single manual validation command runs the legacy script-level staged review flow against representative synthetic PR fixtures without mutating PR, Linear, or remote state:

```sh
.scherzo/workflows/scripts/scherzo-review preflight --output-dir tmp/scherzo-review-preflight
.scherzo/workflows/scripts/scherzo-review preflight --agent-backend fixture --output-dir tmp/scherzo-review-preflight
```

The preflight suite covers small/trivial, medium feature, test-heavy, no-finding, correctness-with-evidence, security-sensitive, performance-sensitive, PR #80-inspired staged-review precision, lane-failure, malformed-lane-output, empty-findings, and duplicate/conflicting synthesis scenarios. Fixture-backed preflight additionally covers an inverted authorization control-condition fixture and a static auth/control suspicion with no trusted reproduction. It validates each generated `ReviewBrief`, `ReviewLaneResult`, `ReviewSynthesis`, and `FinalReviewArtifact`, writes per-scenario command logs, and produces `preflight-manifest.v1.json`. Review findings, including blockers intentionally present in fixtures, do not fail preflight; only workflow execution and artifact-contract problems do.

`preflight-manifest.v1.json` records the selected `agent_backend`, per-lane `lane_runs[].backend`, and a `cutover_readiness` object. Validate the cutover gate with:

```sh
.scherzo/workflows/scripts/scherzo-review validate --artifact tmp/scherzo-review-preflight/preflight-manifest.v1.json --require-cutover-ready
```

That validation succeeds only for a fixture or external manifest whose required semantic scenarios passed, whose required lane runs succeeded with backend metadata, and whose artifacts preserve `remote_mutations: "none"`. A heuristic preflight remains useful for backwards compatibility, but it is not cutover-ready evidence and is never a production implementation-review fallback.

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

The optional live-provider canary is separate from required SelfCI because it may need provider credentials or incur provider cost:

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

After native preparation, the dogfood implementation workflows run the four specialist lanes as Scherzo-managed `kind: agent` steps with required structured output sourced from the generic `submit_review_lane_draft` Pi tool call registered through `.pi/extensions/scherzo-structured-output`. There is no longer a separate review-lane rollback extension shim. Lane draft artifacts are retained by Scherzo under the run artifact store, then `.scherzo/workflows/scripts/scherzo-review verify-evidence`, `normalize-lane-result`, and `synthesize` produce `ReviewLaneResult`, `ReviewSynthesis`, and `FinalReviewArtifact` files under `$SCHERZO_RUN_ROOT/artifacts/review/`. Required JSON outputs retry validation failures once by default (`structured_output.validation_retries`, set to `0` to disable; values above `1` are rejected to keep recovery bounded) with a compact retry prompt that references retained run artifacts instead of replaying diffs, transcripts, or prior full responses. Native review lane `agent_pi_failed` failures caused by `stopReason=error: terminated` are treated as transient and spend the same single retry budget; other agent failures remain lane failures with retained step diagnostics. Workspace mutation checks fail closed before evidence verification. Malformed lane output and evidence-verification failures normalize into failed lane results with retained diagnostics; the native review validation gate blocks publication if the final artifact reports lane failures or execution issues. The existing code-review/review_changes agent still receives the same implementation context and change analysis, plus the native preparation, lane, normalization, synthesis, and final-artifact command outputs, then reads any referenced artifacts before producing the human review summary.

The brief, lane, synthesis, and final review steps write local artifacts only; they do not post PR comments, update Linear, push, rebase, check out PR branches, or alter PR state.

The checked-in dogfood workflows allow the four native review lanes to run concurrently after the clean-workspace checkpoint, bounded by workflow-level `concurrency`. Operators who hit provider quota, rate-limit, or local resource pressure can lower `concurrency` in the workflow YAML to serialize or partially throttle lane execution without changing the lane artifact contract.

## Compatibility and versioning

- Add fields freely when consumers can ignore unknown keys.
- Do not change the meaning or type of existing v1 fields.
- Introduce `schema_version: 2` for breaking changes and keep the v1 schema available for old artifacts.
- Store artifacts in workflow-local output directories or another inspectable local artifact store. Do not rely on Linear comments or PR review comments as the source of truth for the contract.
