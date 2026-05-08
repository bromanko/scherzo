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

## Checked-in workflow integration

The dogfood implementation workflows generate the brief immediately before their existing code-review agent step and store it under the run artifact directory:

```text
$SCHERZO_RUN_ROOT/artifacts/review/<step-id>/
```

This is additive and non-blocking for the current review agent. The existing review agent still receives the same implementation context and change analysis, then uses the generated brief as orientation for future staged lanes when it exists. The brief step writes local artifacts only; it does not post PR comments, update Linear, push, rebase, check out PR branches, or alter PR state.

## Compatibility and versioning

- Add fields freely when consumers can ignore unknown keys.
- Do not change the meaning or type of existing v1 fields.
- Introduce `schema_version: 2` for breaking changes and keep the v1 schema available for old artifacts.
- Store artifacts in workflow-local output directories or another inspectable local artifact store. Do not rely on Linear comments or PR review comments as the source of truth for the contract.
