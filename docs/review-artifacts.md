# Review artifacts v1

Scherzo's staged code-review workflow passes machine-readable artifacts between review steps. This contract is intentionally separate from the Scherzo daemon runtime: review artifacts are local workflow files for agents and operators to inspect, not durable daemon state and not Linear or GitHub comments.

The JSON Schema lives at [`docs/schemas/review-artifacts.v1.schema.json`](schemas/review-artifacts.v1.schema.json). All artifacts use `schema_version: 1` and an `artifact_type` discriminator.

## Artifact types

### `ReviewBrief`

A `ReviewBrief` is the first artifact in the staged review flow. It summarizes the diff or PR and gives later lanes enough shared context to decide what to inspect.

Required fields:

- `schema_version`: `1`.
- `artifact_type`: `review_brief`.
- `generated_at_utc`: timestamp for the brief generation.
- `producer`: tool metadata. The checked-in dry-run producer is `scripts/scherzo-review`.
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

### `ReviewLaneResult`

A `ReviewLaneResult` records a lane execution and embeds its findings.

Required fields:

- `schema_version`: `1`.
- `artifact_type`: `review_lane_result`.
- `lane`: lane metadata (`id`, `name`, `category`, `version`, and optional tool/model data).
- `execution_status`: `state`, start/completion timestamps, and a summary. States are `succeeded`, `failed`, `skipped`, or `blocked`.
- `findings`: list of `ReviewFinding` core objects. An empty list is valid when a lane has no findings.
- `artifacts`: optional local artifact references such as logs, transcripts, or a generated brief.
- `input_brief_ref`: optional reference to the `ReviewBrief` consumed by the lane.

## Dry-run entrypoint

Use `scripts/scherzo-review dry-run` to generate a schema-valid brief without posting comments, updating Linear, pushing branches, checking out a PR, or mutating remote state.

Examples:

```sh
# Current jj change, using @-..@ by default.
scripts/scherzo-review dry-run --output-dir tmp/review-dry-run

# Explicit local jj range.
scripts/scherzo-review dry-run --from main@origin --to @ --output-dir tmp/review-dry-run

# Saved unified diff.
scripts/scherzo-review dry-run --diff-file /path/to/pr.diff --output-dir tmp/review-dry-run

# GitHub PR diff. This uses read-only gh pr diff/view calls only.
scripts/scherzo-review dry-run --pr 74 --repo bromanko/scherzo --output-dir tmp/review-dry-run
```

The command writes:

- `review-brief.v1.json`: `ReviewBrief` artifact.
- `review-lane-result.v1.json`: `ReviewLaneResult` for the brief-generation lane.
- `manifest.v1.json`: local checksums for quick inspection.
- `review-dry-run.log`: bounded local execution log.

It prints the artifact paths as `REVIEW_BRIEF_PATH=...`, `REVIEW_LANE_RESULT_PATH=...`, and `REVIEW_LOG_PATH=...`. It also prints `REVIEW_REMOTE_MUTATIONS=none` as an explicit safety marker.

Validate an artifact with:

```sh
scripts/scherzo-review validate --artifact tmp/review-dry-run/review-brief.v1.json
scripts/scherzo-review validate --artifact tmp/review-dry-run/review-lane-result.v1.json
```

The validator is deliberately minimal and dependency-free. The JSON Schema remains the documentation source of truth for future, richer validators.

## Specialist lane entrypoint

Use `scripts/scherzo-review run-lane` to run one first-version specialist lane against a diff source plus an existing `ReviewBrief`:

```sh
scripts/scherzo-review run-lane \
  --lane correctness \
  --brief tmp/review-dry-run/review-brief.v1.json \
  --diff-file /path/to/pr.diff \
  --output-dir tmp/review-lanes/correctness
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

If a lane fails before producing findings, it still attempts to write `review-lane-<lane-id>.v1.json` with `execution_status.state: "failed"` and a log artifact containing the error, so malformed briefs and tool failures are debuggable from retained workflow artifacts.

The command prints `REVIEW_LANE_RESULT_PATH=...`, `REVIEW_LANE_LOG_PATH=...`, and, on success, `REVIEW_LANE_ANALYSIS_PATH=...` for workflow steps and tests to consume. It is local-only and does not post PR comments, update Linear, push branches, or mutate remote state.

## Checked-in workflow integration

The dogfood implementation workflow generates the brief immediately before review and stores review artifacts under the run artifact directory:

```text
$SCHERZO_RUN_ROOT/artifacts/review/<step-id>/
```

After brief generation, the workflow runs the four specialist lanes as local command steps. The existing code-review agent still receives the same implementation context and change analysis, plus the lane command outputs, then reads any referenced `ReviewLaneResult`, log, and analysis artifacts before producing the human review summary.

The brief and lane steps write local artifacts only; they do not post PR comments, update Linear, push, rebase, check out PR branches, or alter PR state.

## Compatibility and versioning

- Add fields freely when consumers can ignore unknown keys.
- Do not change the meaning or type of existing v1 fields.
- Introduce `schema_version: 2` for breaking changes and keep the v1 schema available for old artifacts.
- Store artifacts in workflow-local output directories or another inspectable local artifact store. Do not rely on Linear comments or PR review comments as the source of truth for the contract.
