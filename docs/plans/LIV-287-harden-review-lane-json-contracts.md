# Harden review-lane structured JSON contracts

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, a Scherzo operator can prove that the native review-lane structured JSON path works before Scherzo claims an implementation issue, creates review workspaces, or spends a full dispatch/review cycle. The operator will be able to run a local offline contract suite that checks schema ownership, fixtures, local validation, and materialization, and an optional live-provider canary that registers the exact Pi tools with the configured provider without creating Linear runs or jj workspaces.

The practical outcome is that provider JSON Schema rejection, weakened schema normalization, and runner-owned metadata mismatches are classified as review infrastructure problems. They are not reported as implementation defects, and they are discovered by cheap commands such as `scripts/scherzo-review-lane-contract offline` rather than by repeated full workflow retries.

## Problem Framing and Constraints

Recent native review attempts failed after otherwise-good implementation work because the review lane asked the model to emit a complete `ReviewLaneDraft` artifact, including deterministic metadata such as schema version, artifact type, timestamp, producer data, lane id, lane name, and input artifact references. Providers rejected some JSON Schema constructs in the tool parameter schemas, and when schemas were sanitized to make providers accept them, important constraints disappeared. Bad or wrong lane metadata then survived provider submission and failed later local validation.

The operator pain is high because a full Linear issue dispatch is currently the test harness for schema and prompt changes. A schema compatibility problem should be caught before Scherzo claims work, and a bad review-lane submission should be treated as review infrastructure or review-agent contract failure, not as proof that the implementation is wrong.

This plan must not replace the staged review system or redesign all structured output. It hardens the review-lane JSON path with a clear split between provider-safe tool arguments and canonical local review artifacts, a standalone contract command, deterministic fixtures, preflight/cache behavior, and runtime classification. It must remain local-first, additive, and reversible.

## Strategy Overview

The selected approach separates two contracts that are currently entangled. The provider-facing contract is a small tool-argument schema used only to register the Pi tool and guide the model. It contains only model-owned fields: `draft_findings`, `review_notes`, `evidence_requests`, and `self_check`. It deliberately omits runner-owned metadata, including `schema_version`, `artifact_type`, `generated_at_utc`, `producer`, `lane`, `input_refs`, and `remote_mutations`. The canonical contract remains the full `ReviewLaneDraft` artifact documented under `docs/schemas/` and validated locally after Scherzo injects deterministic metadata.

This is the right size because it addresses the exact failure modes without weakening the canonical artifact. The provider gets a simpler schema that avoids rejected keywords. The runner still validates the rich canonical artifact with local JSON Schema and semantic checks. The new contract command gives operators a direct harness for offline and live-provider checks, while dispatcher preflight prevents known-bad review infrastructure from consuming implementation capacity.

The work is implemented in small layers: first define and test the contracts, then add materialization and fixtures, then migrate workflows and prompts, then add live canary and dispatcher preflight, and finally wire required offline checks into SelfCI while keeping provider-backed checks optional unless credentials are present.

## Alternatives Considered

The simplest alternative is to keep the current canonical schemas as tool parameter schemas and keep expanding sanitization until the configured provider accepts them. That is insufficient because sanitization removes constraints and still leaves the model responsible for deterministic metadata. It also makes provider compatibility a property discovered only during agent execution.

Another alternative is to remove native structured output and return to script-level or free-form review parsing. That avoids provider tool schema rejection but gives up the existing native review architecture and loses deterministic tool-call capture. The current problem is not that structured output is inherently wrong; it is that provider-safe arguments and canonical local artifacts have been treated as the same object.

A broader alternative is to add a generic transform pipeline to all Scherzo structured output. This may be useful later, but it is larger than needed. This plan uses review-lane-specific materialization first. If the pattern proves useful, a future plan can generalize it.

## Risks and Countermeasures

A provider-safe schema may become too weak and allow malformed payloads. The countermeasure is that provider schemas are never the source of truth. The materialized canonical artifact is validated with the existing rich local schema plus semantic validation, and fixtures cover malformed severities, categories, evidence links, and locations.

The materializer could drift from the canonical schema. The countermeasure is to validate every materialized fixture through `docs/schemas/review-lane-draft.v1.schema.json` and `scripts/scherzo-review validate-structured-output --validator review_lane_draft`. The offline command fails if any lane materializes an artifact that canonical validation rejects.

The live-provider canary could be flaky because real model behavior is not fully deterministic. The countermeasure is to split live results into provider/tool registration failures, model-submission failures, and repair-loop failures. Provider schema rejection is blocking for dispatcher preflight. Model quality failures are reported separately and do not prove the schema is incompatible unless the provider rejects tool registration or tool-call arguments at the transport layer.

Raw workflow validators could keep validating provider submissions as canonical `ReviewLaneDraft` artifacts before materialization. The countermeasure is explicit migration: raw lane agent steps must remove `review_lane_draft_schema`, `review_lane_semantics`, and `review_lane_draft` validators, or replace them only with a submission-shape validator that accepts model-owned fields. Canonical JSON Schema and semantic validation must run from the materialization command output, never from the raw provider submission.

Dispatcher preflight could block useful work because credentials are missing, cache entries are stale, or an operator accidentally enables live-required mode. The countermeasure is an explicit policy: offline preflight is required by default, live transport preflight is skipped unless `SCHERZO_REVIEW_LANE_PREFLIGHT_MODE=live-required`, failed cache entries expire after the configured TTL, and `SCHERZO_REVIEW_LANE_PREFLIGHT_MODE=off` is the rollback switch that restores previous claim behavior while leaving SelfCI offline checks intact.

Workflow migration could break artifact paths expected by existing verification and synthesis commands. The countermeasure is to keep the canonical `ReviewLaneDraft` artifact shape and update the review workflow steps so `verify-evidence`, `normalize-lane-result`, and `synthesize` continue to consume canonical draft files at deterministic paths under the run artifacts directory.

If every native review lane fails for review-infrastructure reasons, Scherzo could still publish a misleading implementation-quality verdict. The countermeasure is explicit synthesis control flow: write a diagnostic review-infrastructure artifact, exit nonzero with a named all-lanes infrastructure code, and map that workflow failure to park/report behavior instead of publishing implementation findings.

## Progress

- [x] (2026-05-14 00:00Z) Drafted this ExecPlan proposal from the Linear ticket and a targeted inspection of the current review-lane structured-output path.
- [x] (2026-05-14 00:30Z) Incorporated adversarial review findings about raw workflow validators, preflight policy, all-lanes review-infrastructure failure handling, and open clarification scope.
- [ ] Implement provider-safe review-lane tool schemas and recursive schema allowlist validation.
- [ ] Implement review-lane submission materialization and canonical local validation.
- [ ] Add offline fixtures for all lanes and all required failure classes.
- [ ] Add the standalone offline and live-provider contract command.
- [ ] Migrate review workflows and prompts to model-owned payloads only.
- [ ] Add dispatcher preflight/cache behavior and runtime review-infrastructure classification.
- [ ] Wire the required offline suite into SelfCI and document optional provider-backed checks.

## Surprises & Discoveries

- Observation: The current native review workflow already uses Pi tool-call structured output for four lanes: `correctness`, `test-quality`, `idioms-maintainability`, and `security-performance`.
  Evidence: `.scherzo/workflows/review-native.yml`, `.scherzo/workflows/implementation.yaml`, and `.scherzo/workflows/execplan-implementation.yaml` define agent lane steps with `structured_output.source.type: pi_tool_call` and lane-specific review-lane draft schema paths.

- Observation: The model is currently required to emit deterministic review-lane metadata.
  Evidence: The lane steps require `schema_version`, `artifact_type`, `generated_at_utc`, `producer`, `lane`, `input_refs`, `draft_findings`, `review_notes`, `evidence_requests`, `self_check`, and `remote_mutations` in the structured output schema block.

- Observation: The current lane agent steps also run canonical structured-output validators against the raw captured tool-call arguments.
  Evidence: `.scherzo/workflows/review-native.yml`, `.scherzo/workflows/implementation.yaml`, and `.scherzo/workflows/execplan-implementation.yaml` include raw `structured_output.validators` entries named `review_lane_draft_schema`, `review_lane_semantics`, or `review_lane_draft` next to the agent `structured_output` blocks. These validators must move off the raw submission path during migration.

- Observation: The checked-in canonical review-lane draft schema contains rich JSON Schema constructs that are appropriate for local validation but risky as provider tool parameter schemas.
  Evidence: `docs/schemas/review-lane-draft.v1.schema.json` uses local-contract constructs such as `$ref`, `$defs`, `allOf`, `enum`, `const`, `not`, nested `anyOf`, and union-style `type` arrays.

- Observation: The current provider compatibility guard is shallow.
  Evidence: `src/scherzo/structured_output_tool_spec.gleam` rejects only top-level `oneOf`, `anyOf`, `allOf`, `enum`, and `not`, and ensures the top-level schema type is `object`; it does not recursively enforce a strict provider-safe keyword allowlist.

- Observation: Existing review helper code already knows how to validate and normalize review artifacts, so the new command can reuse repository conventions instead of inventing a separate review system.
  Evidence: `scripts/scherzo-review` contains `validate_review_lane_draft`, `validate-structured-output`, `verify-evidence`, `normalize-lane-result`, `synthesize`, and existing native preflight helpers.

- Observation: SelfCI is available as a repo-level final gate and currently runs formatting, production lint, custom Scherzo lint, unit tests, and Nix flake checks.
  Evidence: `.config/selfci/ci.sh` invokes `direnv allow .`, `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, `direnv exec . gleam run -m scherzo_lint`, `direnv exec . scherzo-test-unit`, and `nix flake check --print-build-logs`.

## Decision Log

- Decision: Keep the full `ReviewLaneDraft` artifact as the canonical local contract and introduce separate provider-safe tool argument schemas for review-lane submissions.
  Rationale: Provider schemas need to avoid provider-rejected JSON Schema keywords, but local artifacts still need rich validation. Separating the two prevents schema sanitization from weakening canonical review validation.
  Date: 2026-05-14

- Decision: Treat lane metadata, artifact metadata, producer metadata, timestamps, input artifact references, and remote mutation markers as runner-owned fields injected after the model submits its payload.
  Rationale: These values are deterministic from workflow configuration, run context, and prepared review artifacts. Asking the model to reproduce them creates avoidable local validation failures.
  Date: 2026-05-14

- Decision: Add a standalone command named `scripts/scherzo-review-lane-contract` instead of requiring operators to run full workflows for contract checks.
  Rationale: The ticket explicitly needs cheap validation commands that do not create Linear runs or jj workspaces. A standalone script can be used by SelfCI, local operators, dispatcher preflight, and live-provider canaries.
  Date: 2026-05-14

- Decision: Make offline contract validation required in SelfCI and make live-provider validation optional unless credentials and an explicit required-live setting are present.
  Rationale: Offline validation must be deterministic and cheap. Live-provider checks are valuable canaries but depend on network, credentials, provider availability, and model behavior.
  Date: 2026-05-14

- Decision: Remove canonical review-lane draft validators from raw lane agent steps during workflow migration.
  Rationale: Provider submissions intentionally omit runner-owned metadata, so validating them as canonical `ReviewLaneDraft` files before materialization would recreate the failure this plan is meant to remove. Canonical schema and semantic validation must attach to the materialized draft path.
  Date: 2026-05-14

- Decision: Define dispatcher preflight as a policy-gated, cacheable gate with an offline default, optional live-required mode, a 24-hour default TTL, and an explicit `off` rollback mode.
  Rationale: Preflight runs before claim and can halt useful work if misconfigured. A small policy surface and predictable cache behavior make the safety gate reversible and testable.
  Date: 2026-05-14

- Decision: Make all-lanes review-infrastructure failure a named synthesis failure instead of a low-quality implementation verdict.
  Rationale: When every lane fails because the review infrastructure failed, Scherzo has no review signal about the implementation. Publishing a verdict would mislead operators and authors.
  Date: 2026-05-14

- Decision: Close the live-canary and dispatcher-integration clarification items by specifying helper module and gate boundaries in this plan.
  Rationale: Implementation may still fact-check exact call sites in the current tree, but it should not have to design the live probe or pre-claim gate contract from scratch.
  Date: 2026-05-14

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo is a workflow runner that reads workflow YAML files from `.scherzo/workflows/`, runs command and agent steps, captures artifacts under a run root, and reports progress to operators. A native review lane is an agent step that asks a model to call a Pi tool with JSON arguments. Scherzo captures those tool-call arguments as structured output and validates them before later command steps verify evidence and synthesize a final review.

The current review-lane documentation is in `docs/review-artifacts.md`. The canonical aggregate schema is `docs/schemas/review-artifacts.v1.schema.json`. The focused review-lane draft schema is `docs/schemas/review-lane-draft.v1.schema.json`, with lane-specific schema files currently named `docs/schemas/review-lane-draft.correctness.v1.schema.json`, `docs/schemas/review-lane-draft.test-quality.v1.schema.json`, `docs/schemas/review-lane-draft.idioms-maintainability.v1.schema.json`, and `docs/schemas/review-lane-draft.security-performance.v1.schema.json`.

The review workflows that must be migrated are `.scherzo/workflows/review-native.yml`, `.scherzo/workflows/implementation.yaml`, and `.scherzo/workflows/execplan-implementation.yaml`. Each currently defines four lane agent steps and later command steps that call `scripts/scherzo-review verify-evidence`, `scripts/scherzo-review normalize-lane-result`, and `scripts/scherzo-review synthesize`.

The structured-output runtime code is in `src/scherzo/structured_output.gleam`, `src/scherzo/structured_output_source.gleam`, `src/scherzo/structured_output_validator.gleam`, `src/scherzo/structured_output_json_schema.gleam`, `src/scherzo/structured_output_command_validator.gleam`, `src/scherzo/structured_output_tool_spec.gleam`, `src/scherzo/structured_output_metadata.gleam`, and `src/scherzo/workflow_structured_retry.gleam`. The workflow runner that builds tool specs and executes agent steps is `src/scherzo/workflow_run.gleam`. Scheduling state is in `src/scherzo/workflow_scheduler.gleam`. Local workflow execution for native review smoke runs is in `src/scherzo/local_workflow_run.gleam`.

The existing Python review helper is `scripts/scherzo-review`, with reusable Python support under `scripts/scherzo_review/`. The new standalone command should live at `scripts/scherzo-review-lane-contract` and may share code with `scripts/scherzo-review` by moving common review-lane helpers into `scripts/scherzo_review/review_lane_contract.py` or another module under `scripts/scherzo_review/`.

## Preconditions and Verified Facts

The repository uses direnv. In a fresh workspace, if `direnv exec . <command>` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry through direnv.

The working copy was clean before this plan was written, as reported by `$SCHERZO_WORKSPACE_DRIVER status --human`.

The plan file name `docs/plans/LIV-287-harden-review-lane-json-contracts.md` did not collide with an existing `docs/plans/LIV-287-*.md` file during plan authoring.

The currently inspected native review workflows require the model to emit the full canonical metadata-bearing artifact. The currently inspected provider schema compatibility code rejects only a small set of top-level keywords. The currently inspected SelfCI script is `.config/selfci/ci.sh`.

Production lint gates are:

    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

Do not add production `let assert`, `panic`, or `todo` in `src/`. Tests are intentionally outside the production glinter policy.

## Scope Boundaries

In scope: define provider-safe review-lane submission schemas; keep and strengthen canonical local review-lane artifact validation; add materialization that injects deterministic runner metadata; add fixtures for all review lanes; add offline and live-provider contract commands; migrate native review workflows and prompts; add provider schema allowlist enforcement; add dispatcher preflight/cache behavior; classify runtime review-lane structured-output failures as review infrastructure; and wire offline contract validation into SelfCI.

Out of scope: replacing the staged review architecture, changing Linear tracker semantics unrelated to preflight reporting, changing jj workspace drivers, weakening canonical review artifact schemas to match provider limitations, requiring provider credentials for normal unit tests, or using a full implementation workflow as the contract test harness.

The new provider-safe schemas must not replace `docs/schemas/review-lane-draft.v1.schema.json` as the source of truth for retained review artifacts. They are only Pi tool parameter schemas.

The migration should preserve existing local-only safety: contract commands, preflight, and review helpers must not post comments, update Linear, push branches, or mutate remote state unless the dispatcher preflight reporting path explicitly decides to park or comment on an issue before claim.

## Milestones

Milestone 1 establishes contract ownership and schema allowlist enforcement. At the end of this milestone, the repository contains provider-safe schema files for each lane and tests that fail if any provider schema uses a disallowed keyword. This milestone comes first because provider rejection is the highest-risk unknown and can be tested without model calls.

Milestone 2 adds materialization and fixture validation without changing production workflows. At the end of this milestone, `scripts/scherzo-review-lane-contract check-schema` validates every provider schema and `scripts/scherzo-review-lane-contract materialize` can read each lane's valid fixture, reject bad submissions through the helper path, inject deterministic metadata for valid submissions, and validate the resulting canonical `ReviewLaneDraft` artifacts locally. Full `offline --workflow ...` is still expected to fail until Milestone 3 because workflow files still point at canonical schemas or canonical raw validators. This proves the canonical-vs-provider split before production workflow migration.

Milestone 3 migrates the native review workflows and prompts. At the end of this milestone, lane prompts ask the model to submit only model-owned fields, lane agent steps register provider-safe schemas, raw agent-step validators no longer validate submissions as canonical drafts, and downstream review commands consume materialized canonical drafts. This is where operator-visible behavior changes, so it follows the offline contract suite.

Milestone 4 adds the live-provider canary and dispatcher preflight/cache. At the end of this milestone, an operator can run a live probe that registers the exact review-lane tools with the configured provider without creating a Linear run or jj workspace, and the dispatcher can refuse to claim implementation work when the configured preflight policy says review-tool compatibility is known to fail. The preflight cache has a defined JSON shape, cache key, expiry, and rollback mode.

Milestone 5 completes runtime classification, SelfCI integration, and documentation. At the end of this milestone, structured-output failures in review lanes are reported as review infrastructure problems, all-lanes review-infrastructure failures stop publication with a named diagnostic, offline contract checks run in SelfCI, provider-backed checks are optional based on credentials, and acceptance commands demonstrate the hardened path without full dispatch.

## Plan of Work

Create `scripts/scherzo-review-lane-contract` as a Python command-line entry point. Keep it dependency-light and consistent with `scripts/scherzo-review`. Put reusable functions in `scripts/scherzo_review/review_lane_contract.py` so tests and the dispatcher-facing probe can share them. The command owns provider-schema allowlist checks, fixture execution, materialization, canonical validation orchestration, and live-provider canary reporting.

Add provider-safe schemas under a new directory `docs/schemas/provider/`. Create one schema per lane: `docs/schemas/provider/review-lane-draft.correctness.v1.schema.json`, `docs/schemas/provider/review-lane-draft.test-quality.v1.schema.json`, `docs/schemas/provider/review-lane-draft.idioms-maintainability.v1.schema.json`, and `docs/schemas/provider/review-lane-draft.security-performance.v1.schema.json`. These are Pi tool parameter schemas, not retained review artifacts. They must have top-level `type: object`, `additionalProperties: false`, and required model-owned fields only: `draft_findings`, `review_notes`, `evidence_requests`, and `self_check`. They must not require or allow `schema_version`, `artifact_type`, `generated_at_utc`, `producer`, `lane`, `input_refs`, or `remote_mutations`.

Use a strict provider-schema keyword allowlist. The initial allowlist is `type`, `description`, `properties`, `required`, `additionalProperties`, `items`, `minLength`, `maxLength`, `minimum`, `maximum`, `minItems`, `maxItems`, and `pattern`. The provider schemas must not contain `$schema`, `$id`, `$defs`, `$ref`, `oneOf`, `anyOf`, `allOf`, `not`, `enum`, `const`, `if`, `then`, `else`, `dependentRequired`, `dependentSchemas`, `unevaluatedProperties`, or union-style `type` arrays. If implementation discovers that the configured provider rejects one of the initially allowed keywords, remove that keyword from the allowlist and record the evidence in the Decision Log before updating schemas.

Update `src/scherzo/structured_output_tool_spec.gleam` so provider compatibility validation is recursive and allowlist-based, not only a top-level rejection of several keywords. Add a public function such as `validate_provider_schema_keywords(schema: json_value.JsonValue, schema_path: String) -> Result(Nil, ToolSpecError)` and call it from `provider_compatible_parameters_schema`. Preserve the existing top-level object requirement. Error messages must include the schema path, JSON pointer-like location, and disallowed keyword, for example `provider schema docs/schemas/provider/review-lane-draft.correctness.v1.schema.json contains disallowed keyword enum at properties.draft_findings.items.properties.severity`.

Add or update tests in `test/structured_output_tool_spec_test.gleam`. Include a passing test for a nested provider-safe object schema and failing tests for nested `enum`, nested `const`, nested `allOf`, nested `$ref`, and union-style `type` arrays. These tests should fail before the recursive allowlist change because the current implementation checks only top-level incompatible keywords.

Define the review-lane submission payload type in the new Python helper. A valid submission has this shape:

    {
      "draft_findings": [],
      "review_notes": [],
      "evidence_requests": [],
      "self_check": {
        "summary": "Inspected the diff and found no concrete findings."
      }
    }

The materializer builds a canonical `ReviewLaneDraft` by injecting:

    schema_version: 1
    artifact_type: "review_lane_draft"
    generated_at_utc: current UTC time from the runner
    producer: {"name": "scherzo", "version": "1", "mode": "native-review-lane"}
    lane: the lane metadata from the configured lane table
    input_refs: review brief, diff, changed files, validation status, and context manifest references from the prepared review directory
    remote_mutations: "none"

The model-submitted arrays and `self_check` are copied from the submission after local validation. The materializer must reject unexpected metadata fields in the provider submission. If a submission includes `schema_version`, `artifact_type`, `generated_at_utc`, `producer`, `lane`, `input_refs`, or `remote_mutations`, fail with an error code such as `review_lane_submission_unexpected_runner_metadata`.

Keep canonical validation in the local path. After materialization, validate the artifact against `docs/schemas/review-lane-draft.v1.schema.json` or the lane-specific canonical schema, then run the existing semantic validator through `scripts/scherzo-review validate-structured-output --validator review_lane_draft`. Add semantic checks if they are missing: draft finding ids are unique within a lane; each evidence request references an existing draft finding id; each draft finding evidence request id exists; severity values are one of `info`, `low`, `medium`, `high`, or `critical`; review-note categories are one of the documented canonical categories; finding locations and evidence target paths are repository-relative; and location `start_line` is not greater than `end_line` when both are present.

Add fixture directories under `test/fixtures/review-lane-contract/`. Use one directory per lane:

    test/fixtures/review-lane-contract/correctness/
    test/fixtures/review-lane-contract/test-quality/
    test/fixtures/review-lane-contract/idioms-maintainability/
    test/fixtures/review-lane-contract/security-performance/

Each lane directory must contain at least these provider-submission fixture files: `valid-minimal.arguments.json`, `missing-required-field.arguments.json`, `unexpected-runner-metadata.arguments.json`, `invalid-severity.arguments.json`, `invalid-category.arguments.json`, `invalid-evidence-link.arguments.json`, `malformed-location.arguments.json`, and `malformed-evidence-target.arguments.json`. If any lane keeps a lane-like model-owned field during implementation, also add `wrong-lane-id.arguments.json`; otherwise `unexpected-runner-metadata.arguments.json` must include a wrong `lane` object and prove that lane metadata is rejected before materialization.

The fixture manifest for each lane should be `manifest.v1.json` and list expected outcomes. A representative manifest entry is:

    {
      "fixture": "invalid-severity.arguments.json",
      "expect": "fail",
      "code": "review_lane_submission_canonical_validation_failed",
      "contains": "severity"
    }

The offline command must read these manifests rather than hard-coding expected filenames. That makes it easy to add future fixtures without changing the command.

Implement the command-line interface as follows:

    scripts/scherzo-review-lane-contract check-schema \
      --schema docs/schemas/provider/review-lane-draft.correctness.v1.schema.json

    scripts/scherzo-review-lane-contract materialize \
      --lane correctness \
      --submission test/fixtures/review-lane-contract/correctness/valid-minimal.arguments.json \
      --prepare-dir test/fixtures/review-lane-contract/prepared-review \
      --output tmp/scherzo-review-lane-contract/correctness/review-lane-draft.v1.json

    scripts/scherzo-review-lane-contract offline \
      --workflow .scherzo/workflows/review-native.yml \
      --fixtures test/fixtures/review-lane-contract \
      --output-dir tmp/scherzo-review-lane-contract/offline

    scripts/scherzo-review-lane-contract live \
      --workflow .scherzo/workflows/review-native.yml \
      --output-dir tmp/scherzo-review-lane-contract/live \
      --skip-if-missing-credentials

    scripts/scherzo-review-lane-contract preflight-cache-key \
      --workflow .scherzo/workflows/implementation.yaml

The `offline` subcommand must check every provider schema with the allowlist, verify that the workflow lane steps point at provider schemas rather than canonical schemas, verify that raw lane agent steps do not run canonical `ReviewLaneDraft` validators against captured submissions, verify that each lane has a materialization command before evidence verification, run all fixtures, materialize valid submissions, run canonical validation, run semantic validation, and write `contract-report.v1.json`. The report must include `schema_status`, `workflow_status`, `validator_status`, `fixture_status`, `materialization_status`, `canonical_validation_status`, `remote_mutations: "none"`, and per-lane results. If a raw agent step still has a validator named `review_lane_draft_schema`, `review_lane_semantics`, or `review_lane_draft`, or a command validator invoking `scripts/scherzo-review validate-structured-output --validator review_lane_draft` against the captured submission artifact, `offline` must fail with code `review_lane_workflow_raw_validator_targets_canonical_draft`.

The `live` subcommand must not create Linear runs or jj workspaces. It should load the workflow, build the same Pi tool specs that the runner would register for the review-lane steps, and start a small provider canary that uses a temporary run root under `tmp/`. The canary has two phases. The registration phase sends a minimal prompt that asks the provider to call the configured review-lane tool with a valid minimal payload for each lane. The repair phase deliberately feeds the local structured-output validator an invalid captured payload, constructs the same repair prompt produced by `src/scherzo/workflow_structured_retry.gleam`, and asks the provider to repair by calling the same tool. The report must distinguish `provider_tool_registration_failed`, `provider_tool_call_failed`, `repair_loop_failed`, `model_payload_invalid`, and `skipped_missing_credentials`.

Implement the live probe through a fixed helper boundary instead of a full workflow run. Add `src/scherzo/review_lane_live_probe.gleam` with a `main` function and a pure `probe_workflow` function. The Python `live` subcommand invokes it as `direnv exec . gleam run -m scherzo/review_lane_live_probe -- --workflow <workflow> --output-dir <output-dir> --skip-if-missing-credentials`. The helper must load the workflow, reuse the same provider configuration and `structured_output_tool_spec.for_step` path used by lane agent execution, register the review-lane tools, perform the registration and repair phases, and write `live-probe-report.v1.json`. If the current tree already exposes a lower-level reusable runner function, call it from this helper; if not, this helper is the reusable boundary. Do not fall back to a full Linear dispatch or a jj workspace.

Migrate `.scherzo/workflows/review-native.yml`, `.scherzo/workflows/implementation.yaml`, and `.scherzo/workflows/execplan-implementation.yaml`. For each lane agent step, change `structured_output.source.parameters_schema_path` from the canonical lane schema to the matching provider schema under `docs/schemas/provider/`. Prefer the review-specific tool name `submit_review_lane_draft` for the migrated lane steps. During migration, keep report readers and helper code tolerant of the old `submit_structured_output` name so retained historical artifacts and older smoke tests remain understandable.

For each raw lane agent step, remove the canonical `structured_output.validators` entries named `review_lane_draft_schema`, `review_lane_semantics`, or `review_lane_draft`. If the workflow runtime requires a validator list, replace them only with a submission-shape validator named `review_lane_submission_shape` that checks the four model-owned fields and rejects runner-owned metadata. Do not invoke `scripts/scherzo-review validate-structured-output --validator review_lane_draft` against the raw captured submission artifact. Canonical JSON Schema validation and `review_lane_draft` semantic validation belong to the materialization command output.

For each lane, change `artifact_name` to make clear that the captured tool arguments are a submission, for example `correctness_submission`. Add a command step after each lane agent step and before evidence verification that materializes the canonical draft and runs canonical validation. The materialization command should write a deterministic canonical draft path such as `$SCHERZO_RUN_ROOT/artifacts/review/lanes/correctness/review-lane-draft.v1.json`. Update `verify-evidence` and `normalize-lane-result` command arguments to read that canonical draft path instead of reading the raw structured-output capture from the agent attempt directory.

Update `.scherzo/workflows/prompts/review-native-correctness.md`, `.scherzo/workflows/prompts/review-native-test-quality.md`, `.scherzo/workflows/prompts/review-native-idioms-maintainability.md`, and `.scherzo/workflows/prompts/review-native-security-performance.md`. Each prompt must tell the model to call the configured Pi tool exactly once with only `draft_findings`, `review_notes`, `evidence_requests`, and `self_check`. Each prompt must explicitly say not to include `schema_version`, `artifact_type`, `generated_at_utc`, `producer`, `lane`, `input_refs`, or `remote_mutations`, because Scherzo injects them.

Update `docs/review-artifacts.md` to document the two-layer contract. Define `ReviewLaneSubmission` as the provider-facing, model-owned tool argument object. Define `ReviewLaneDraft` as the canonical, retained artifact produced after runner metadata injection and local validation. Include the offline and live contract commands and state that the contract suite is the test harness for review-lane schema, prompt, and provider compatibility changes.

Add tests for the script command. If the repository convention remains Gleam-driven tests, create `test/review_lane_contract_test.gleam` that shells out to the script with fixture directories under `test/fixtures/review-lane-contract/`. The tests should assert exit codes and report contents for all fixture classes. If Python unit tests are already available in the local test wrapper, use Python tests for pure helper functions and a Gleam smoke test for the command entry point. In either case, the unit suite must exercise the same command paths that operators will run.

Add dispatcher preflight/cache support in production Gleam. Create `src/scherzo/review_lane_preflight.gleam` to scan a loaded `workflow_dag.WorkflowDag` for review-lane structured-output steps. It should build provider tool specs through `structured_output_tool_spec.for_step`, validate provider schemas recursively, compute schema digests, and produce a preflight result. Add `src/scherzo/review_lane_preflight_policy.gleam` for the policy surface. The policy fields are `mode`, `cache_ttl_seconds`, `park_on_failure`, and `strict_live_model_checks`. The default policy is `mode: OfflineRequired`, `cache_ttl_seconds: 86400`, `park_on_failure: True`, and `strict_live_model_checks: False`. Operators may override these with `SCHERZO_REVIEW_LANE_PREFLIGHT_MODE=off|offline|required-live`, `SCHERZO_REVIEW_LANE_PREFLIGHT_CACHE_TTL_SECONDS=<seconds>`, `SCHERZO_REVIEW_LANE_PREFLIGHT_PARK_ON_FAILURE=0|1`, and `SCHERZO_REVIEW_LANE_PREFLIGHT_STRICT_LIVE_MODEL_CHECKS=0|1`.

Persist the preflight cache under the daemon state root as `review-lane-contract-cache.v1.json`; if the current daemon state module has a more specific cache directory convention, use that convention but keep the filename stable. Cache entries are keyed by workflow id, workflow fingerprint, provider name, model name, tool name, provider schema paths and digests, checker version, and preflight mode. A cache entry is usable only when `checked_at_utc` is within `cache_ttl_seconds` and all key material matches. A failed unexpired entry blocks claim only when its `blocking` field is `true`. Deleting the cache file is safe and forces a fresh preflight.

The preflight action table is fixed. In `off` mode, dispatcher preflight is skipped and claim behavior matches the pre-LIV-287 path, but SelfCI still runs offline contract validation. In `offline` mode, local schema/workflow validation failure blocks claim and writes `review_infrastructure_preflight_failed`; live credentials are ignored, missing credentials do not block, and live cache entries do not affect claim. In `required-live` mode, offline failure blocks claim; missing credentials block claim with code `review_lane_live_credentials_missing`; provider tool registration or transport-level tool-call failure blocks claim; `model_payload_invalid` and `repair_loop_failed` are recorded as warnings unless `strict_live_model_checks` is true. When a blocking failure occurs and `park_on_failure` is true, the dispatcher parks or comments through the configured tracker/handoff policy before leaving the issue unclaimed. When `park_on_failure` is false, it writes the report and leaves the issue unclaimed without tracker mutation.

Wire preflight before claim through a fixed gate. Add `src/scherzo/review_lane_preflight_gate.gleam` with a function such as `before_claim(workflow, provider_config, tracker_policy, state_root) -> ClaimGateResult`. The gate must run after workflow/config load and before any handoff claim state transition, workspace preparation, or issue-specific implementation agent step. The implementation agent should fact-check the current claim call site and call the gate there; likely integration points include `src/scherzo/orchestrator/schedule_core.gleam`, `src/scherzo/workflow_run.gleam`, and the tracker handoff code that moves issues into claim states. The design decision is not open: no claim and no workspace preparation may happen after a blocking preflight failure.

Runtime fallback behavior must classify review-lane structured-output failures separately from implementation review findings. Update `scripts/scherzo-review normalize-lane-result` and related helper functions so missing, malformed, provider-rejected, or locally invalid lane submissions produce a `ReviewLaneResult` with `execution_status.state: "failed"`, `execution_status.reason: "review_infrastructure_failure"`, an execution summary beginning with `review infrastructure failure:`, and an artifact reference to the structured-output error metadata when available. Synthesis must place these failures under `execution_issues` and must not convert them into implementation findings.

Define exact all-lanes failure control flow in `scripts/scherzo-review synthesize`. If at least one lane succeeds, synthesis exits 0 and includes any review-infrastructure lane failures under `execution_issues`. If every native lane result has `execution_status.state: "failed"` and `execution_status.reason: "review_infrastructure_failure"`, synthesis must still write a diagnostic artifact such as `review-infrastructure-failure.v1.json` containing `code: "review_infrastructure_all_lanes_failed"`, lane ids, lane result paths, and the structured-output error references, then exit with status code `42`. Downstream publish/comment steps must depend on successful synthesis and therefore must not publish an implementation-quality verdict after exit 42. The dispatcher maps this named failure to the same park/report path as preflight review-infrastructure failures.

Update `src/scherzo/workflow_structured_retry.gleam` only as needed for the new model-owned submission contract. Its retry prompt should stop reminding the model to emit runner-owned metadata. It should remind the model to call `submit_review_lane_draft` with only model-owned fields and to use repository-relative paths in locations and evidence targets.

Update `.config/selfci/ci.sh` to run the offline contract suite after Scherzo custom lint and before the unit test suite:

    run_step "review lane contract offline" direnv exec . scripts/scherzo-review-lane-contract offline --workflow .scherzo/workflows/review-native.yml --fixtures test/fixtures/review-lane-contract --output-dir tmp/scherzo-review-lane-contract/selfci

Do not add the live-provider check to the default required SelfCI path. Document the optional live check in `docs/review-artifacts.md` and in the command help. Use an environment flag such as `SCHERZO_REVIEW_LANE_CONTRACT_LIVE=1` or a direct operator command to run the live check when credentials are available.

[CLARIFY] Before adding the optional live check to any hosted CI job, confirm the repository's credential policy and provider cost policy. This plan requires the command and documentation, but it does not require provider-backed checks to run in ordinary SelfCI.

## Concrete Steps

1. From the repository root, run the current status command and confirm the implementation workspace is clean:

       $SCHERZO_WORKSPACE_DRIVER status --human

   Expect a clean working copy or only intentional implementation changes.

2. Add `scripts/scherzo_review/review_lane_contract.py` with pure helpers for lane metadata, provider schema keyword traversal, submission validation, metadata injection, canonical validation command invocation, fixture manifest loading, and report writing.

3. Add executable `scripts/scherzo-review-lane-contract` that imports the helper module and implements `check-schema`, `materialize`, `offline`, `live`, and `preflight-cache-key` subcommands.

4. Create `docs/schemas/provider/` and add the four provider-safe lane schemas. Keep them free of disallowed keywords and runner-owned metadata.

5. Add `test/fixtures/review-lane-contract/prepared-review/` with minimal prepared-review artifacts needed for `input_refs`: `review-brief.v1.json`, `diff.patch`, `changed-files.v1.json`, `validation-status.v1.json`, and `context-manifest.v1.json`. Keep all paths repository-relative inside the fixture JSON.

6. Add the per-lane fixture directories and `manifest.v1.json` files under `test/fixtures/review-lane-contract/`.

7. Run the offline command before workflow migration to prove the expected red phase. It should fail because workflows still point at canonical schemas or because the command is new and not yet complete:

       direnv exec . scripts/scherzo-review-lane-contract offline --workflow .scherzo/workflows/review-native.yml --fixtures test/fixtures/review-lane-contract --output-dir tmp/scherzo-review-lane-contract/offline

   Record the failure in the implementation notes if it differs from the expected not-yet-migrated failure.

8. Update `src/scherzo/structured_output_tool_spec.gleam` with recursive provider schema allowlist validation and clear error reporting.

9. Add focused tests in `test/structured_output_tool_spec_test.gleam` for nested allowed keywords and nested disallowed keywords.

10. Run the targeted tool-spec tests through the unit suite selector available in the repository. If the custom unit wrapper does not support a single test file, run the full unit wrapper:

       direnv exec . scherzo-test-unit

   Expect the new tool-spec tests to pass after the allowlist implementation.

11. Complete the pre-migration helper path until `check-schema` passes for each provider schema and `materialize` passes for each lane's `valid-minimal.arguments.json` fixture. Do not require full `offline --workflow ...` to pass yet; the full offline command must continue to fail before workflow migration because raw workflows still point at canonical schemas or canonical raw validators.

12. Migrate `.scherzo/workflows/review-native.yml` lane steps to provider schemas and materialization command steps. Remove canonical raw `structured_output.validators` from lane agent steps, or replace them only with `review_lane_submission_shape`. Update downstream draft paths for evidence verification and normalization.

13. Apply the same workflow migration to `.scherzo/workflows/implementation.yaml` and `.scherzo/workflows/execplan-implementation.yaml`.

14. Update the four `.scherzo/workflows/prompts/review-native-*.md` prompts so they describe only the model-owned submission payload and explicitly forbid runner-owned metadata.

15. Update `docs/review-artifacts.md` with the two-layer contract and the new command examples.

16. Run the offline contract suite for each migrated workflow:

       direnv exec . scripts/scherzo-review-lane-contract offline --workflow .scherzo/workflows/review-native.yml --fixtures test/fixtures/review-lane-contract --output-dir tmp/scherzo-review-lane-contract/review-native
       direnv exec . scripts/scherzo-review-lane-contract offline --workflow .scherzo/workflows/implementation.yaml --fixtures test/fixtures/review-lane-contract --output-dir tmp/scherzo-review-lane-contract/implementation
       direnv exec . scripts/scherzo-review-lane-contract offline --workflow .scherzo/workflows/execplan-implementation.yaml --fixtures test/fixtures/review-lane-contract --output-dir tmp/scherzo-review-lane-contract/execplan-implementation

   Expect each command to exit 0 and write a `contract-report.v1.json` with all lanes passing expected fixture outcomes and `remote_mutations: "none"`.

17. Implement `scripts/scherzo-review-lane-contract live`. Run it in skip-safe mode:

       direnv exec . scripts/scherzo-review-lane-contract live --workflow .scherzo/workflows/review-native.yml --output-dir tmp/scherzo-review-lane-contract/live --skip-if-missing-credentials

   Expect either a skipped report with `skipped_missing_credentials` or a successful report that shows provider tool registration and repair-loop status.

18. Add `src/scherzo/review_lane_preflight.gleam`, `src/scherzo/review_lane_preflight_policy.gleam`, and tests for cache key calculation, cache JSON encoding/decoding, TTL expiry, schema failure reporting, cache hit behavior, missing-credentials behavior, and cache invalidation when a provider schema digest changes.

19. Wire `src/scherzo/review_lane_preflight_gate.gleam` into the dispatcher path before issue claim or workspace preparation. Add tests that simulate offline failure, live-required missing credentials, unexpired blocking failed cache, `mode=off`, and `park_on_failure=0`; assert no claim or workspace preparation on blocking failures and an operator-visible `review_infrastructure_preflight_failed` report.

20. Update runtime fallback classification in `scripts/scherzo-review`, `src/scherzo/workflow_structured_retry.gleam`, synthesis, and any step-artifact reporting needed so review-lane structured-output failures become review infrastructure execution issues, not implementation findings. Add the all-lanes infrastructure failure exit 42 and diagnostic artifact.

21. Update `.config/selfci/ci.sh` with the required offline contract step.

22. Run focused validation:

       direnv exec . scripts/scherzo-review-lane-contract offline --workflow .scherzo/workflows/review-native.yml --fixtures test/fixtures/review-lane-contract --output-dir tmp/scherzo-review-lane-contract/final
       direnv exec . gleam format --check src test
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint
       direnv exec . scherzo-test-unit

   Expect all commands to exit 0.

23. Run full validation before publishing:

       direnv exec . selfci check --base main@origin --candidate @ --print-output

   Expect SelfCI to pass, including the new offline review-lane contract step. If SelfCI is unavailable, run the commands from `.config/selfci/ci.sh` individually and record which were run.

24. Optional operator canary with real credentials:

       direnv exec . scripts/scherzo-review-lane-contract live --workflow .scherzo/workflows/implementation.yaml --output-dir tmp/scherzo-review-lane-contract/live-implementation --skip-if-missing-credentials

   This is not required for ordinary SelfCI, but it is the command operators should use before enabling a new provider, model, prompt, or review-lane schema in dogfood dispatch.

25. Commit point: after offline contract checks, targeted tests, lint, and SelfCI pass, create one logical implementation commit. If running inside a Scherzo implementation workflow that creates the final commit automatically, do not manually create a jj commit.

## Testing and Falsifiability

The schema allowlist tests must prove that provider schemas are safe at every nesting level. Add tests in `test/structured_output_tool_spec_test.gleam` that call `structured_output_tool_spec.for_step` or the new allowlist function with nested schema objects. A schema with nested `description`, `properties`, `items`, and `required` passes. A schema with nested `enum`, nested `const`, nested `allOf`, nested `$ref`, or `type: ["string", "null"]` fails with `structured_output_tool_spec_provider_incompatible_schema` and a message naming the disallowed keyword location.

The fixture tests must prove the materializer rejects bad provider submissions and accepts valid minimal submissions for every lane. For each lane, `valid-minimal.arguments.json` must materialize into a canonical artifact with the correct injected lane id and `remote_mutations: "none"`. `missing-required-field.arguments.json` must fail before materialization. `unexpected-runner-metadata.arguments.json` must fail because the model tried to provide runner-owned metadata. `invalid-severity.arguments.json`, `invalid-category.arguments.json`, `invalid-evidence-link.arguments.json`, `malformed-location.arguments.json`, and `malformed-evidence-target.arguments.json` must fail local canonical or semantic validation with the expected code recorded in the manifest.

The workflow migration tests must prove that production workflow files no longer point `parameters_schema_path` at canonical schemas and no longer validate raw submissions as canonical drafts. Add a test that parses `.scherzo/workflows/review-native.yml`, `.scherzo/workflows/implementation.yaml`, and `.scherzo/workflows/execplan-implementation.yaml`; finds the four review-lane agent steps; and asserts each `parameters_schema_path` starts with `docs/schemas/provider/`, each raw lane step has no validators named `review_lane_draft_schema`, `review_lane_semantics`, or `review_lane_draft`, each raw lane step has no command validator invoking `scripts/scherzo-review validate-structured-output --validator review_lane_draft` against the captured submission, each lane has a materialization command step, and downstream `verify-evidence` and `normalize-lane-result` commands consume the materialized canonical draft path.

The live-provider canary is falsifiable in two ways. If the provider rejects a tool schema, `live` must fail with `provider_tool_registration_failed` and the dispatcher preflight must refuse to claim work when live preflight is required. If the provider accepts the schema but the model does not produce a valid minimal payload, `live` must report `model_payload_invalid` or `repair_loop_failed`; that failure is useful canary evidence but should not be confused with local schema rejection.

The dispatcher preflight tests must prove no implementation claim happens after a blocking review-infrastructure preflight failure and that rollback policy works. Use a fake tracker and fake workspace-preparation dependency. Force `review_lane_preflight` to return a provider schema failure, a missing-credentials result in `required-live` mode, an unexpired blocking failed cache entry, and a nonblocking live warning in default mode. Assert the fake tracker did not receive a claim transition for blocking failures, the fake workspace driver was not invoked, the operator report contains `review_infrastructure_preflight_failed` and the disallowed keyword or provider rejection reason, `park_on_failure=0` suppresses tracker mutation, and `mode=off` allows the old claim path.

The runtime fallback tests must prove review infrastructure failures do not become implementation findings. Provide a malformed lane submission, run the materialization or normalization path, and assert the resulting `ReviewLaneResult` has `execution_status.state: "failed"`, `execution_status.reason: "review_infrastructure_failure"`, has no implementation finding for the malformed JSON itself, and is represented in synthesis `execution_issues`. Add an all-lanes case where every lane result is a review-infrastructure failure; assert `scripts/scherzo-review synthesize` writes `review-infrastructure-failure.v1.json`, exits 42, and no publish/comment step treats the result as an implementation-quality verdict.

The plan is disproved if any of these are true after implementation: a full implementation workflow is still required to test provider schema acceptance; a provider schema under `docs/schemas/provider/` contains a disallowed keyword; a model can influence lane id or artifact type by adding metadata to a submission; a review-lane schema failure is published as an implementation finding; or SelfCI can pass while the offline review-lane contract suite fails.

## Validation and Acceptance

Acceptance is operator-visible. From the repository root, the required no-dispatch validation command is:

    direnv exec . scripts/scherzo-review-lane-contract offline --workflow .scherzo/workflows/review-native.yml --fixtures test/fixtures/review-lane-contract --output-dir tmp/scherzo-review-lane-contract/acceptance

It must exit 0 and write `tmp/scherzo-review-lane-contract/acceptance/contract-report.v1.json`. The report must show all four lanes, provider schema allowlist success, expected fixture pass/fail outcomes, successful materialization for valid fixtures, successful canonical validation for materialized artifacts, and `remote_mutations: "none"`.

The required migrated-workflow validation commands are:

    direnv exec . scripts/scherzo-review-lane-contract offline --workflow .scherzo/workflows/implementation.yaml --fixtures test/fixtures/review-lane-contract --output-dir tmp/scherzo-review-lane-contract/acceptance-implementation
    direnv exec . scripts/scherzo-review-lane-contract offline --workflow .scherzo/workflows/execplan-implementation.yaml --fixtures test/fixtures/review-lane-contract --output-dir tmp/scherzo-review-lane-contract/acceptance-execplan-implementation

Both must exit 0 without creating Linear runs or jj workspaces.

The required local code validation commands are:

    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint
    direnv exec . scherzo-test-unit

All must exit 0. The full final gate is:

    direnv exec . selfci check --base main@origin --candidate @ --print-output

It must pass and include the offline review-lane contract step.

The optional live-provider acceptance command is:

    direnv exec . scripts/scherzo-review-lane-contract live --workflow .scherzo/workflows/review-native.yml --output-dir tmp/scherzo-review-lane-contract/live-acceptance --skip-if-missing-credentials

With no credentials, it must exit 0 only if it writes a skipped report that clearly says `skipped_missing_credentials`. With credentials and a configured provider, it must register the exact review-lane tools, exercise the repair loop, and report either success or a classified provider/model failure without creating Linear runs or jj workspaces.

## Rollout, Recovery, and Idempotence

Roll out the change additively. Add provider schemas and the offline command before changing workflows. Keep canonical schema files and canonical validators available for materialized drafts. Keep existing script-level review helpers available. During migration, allow report readers to recognize both `submit_structured_output` and `submit_review_lane_draft` so retained artifacts remain debuggable.

If workflow migration causes problems, rollback is straightforward: revert the workflow files to their previous `parameters_schema_path` values, restore the old raw canonical validators, and remove the materialization steps. Because canonical schemas are preserved and provider schemas are additive, reverting workflow references does not require data migration.

The contract command is idempotent. It writes reports under the requested `tmp/` output directory and may overwrite previous reports in that directory. It must not mutate remote state. The dispatcher preflight cache is safe to delete; deleting it only forces preflight to run again. To recover from a bad dispatcher preflight rollout, set `SCHERZO_REVIEW_LANE_PREFLIGHT_MODE=off` for the daemon to restore previous claim behavior, or set `SCHERZO_REVIEW_LANE_PREFLIGHT_PARK_ON_FAILURE=0` to keep the no-claim safety gate while suppressing tracker parking/commenting. Setting `SCHERZO_REVIEW_LANE_PREFLIGHT_CACHE_TTL_SECONDS=0` or deleting `review-lane-contract-cache.v1.json` forces a fresh result after a provider or schema fix.

When preflight fails before claim, Scherzo should leave implementation work unclaimed and produce an operator-visible report. If configured tracker parking is available and `park_on_failure` is true, parking or commenting should be a controlled operator signal, not a partial implementation run. Retrying after a schema fix or provider config fix should invalidate the cache because schema digests, workflow fingerprints, or provider settings changed.

Runtime recovery should prefer a valid failed lane artifact over a missing artifact. A failed `ReviewLaneResult` with a review-infrastructure execution status lets synthesis and operators understand that the review system failed, while preserving the distinction between review infrastructure and implementation findings.

## Artifacts and Notes

Expected minimal provider submission fixture:

    {
      "draft_findings": [],
      "review_notes": [],
      "evidence_requests": [],
      "self_check": {
        "summary": "No concrete finding after inspecting the prepared review inputs."
      }
    }

Expected injected canonical fields for the same submission:

    {
      "schema_version": 1,
      "artifact_type": "review_lane_draft",
      "generated_at_utc": "<runner-generated-utc-timestamp>",
      "producer": {
        "name": "scherzo",
        "version": "1",
        "mode": "native-review-lane"
      },
      "lane": {
        "id": "correctness",
        "name": "Correctness reviewer",
        "category": "correctness",
        "version": "1"
      },
      "input_refs": [
        {
          "artifact_type": "review_brief",
          "path": "artifacts/review/prepare_review/review-brief.v1.json"
        }
      ],
      "draft_findings": [],
      "review_notes": [],
      "evidence_requests": [],
      "self_check": {
        "summary": "No concrete finding after inspecting the prepared review inputs."
      },
      "remote_mutations": "none"
    }

Expected provider-schema rejection message shape:

    structured_output_tool_spec_provider_incompatible_schema: provider schema docs/schemas/provider/review-lane-draft.correctness.v1.schema.json contains disallowed keyword enum at properties.draft_findings.items.properties.severity

Expected review-infrastructure classification text:

    review_infrastructure_preflight_failed: review lane provider schema rejected before implementation claim

Expected preflight cache entry shape:

    {
      "schema_version": 1,
      "entries": [
        {
          "cache_key": "<stable-cache-key>",
          "workflow_id": "implementation",
          "workflow_fingerprint": "<workflow-fingerprint>",
          "provider_name": "<provider>",
          "model_name": "<model>",
          "tool_names": ["submit_review_lane_draft"],
          "schema_digests": {
            "docs/schemas/provider/review-lane-draft.correctness.v1.schema.json": "<sha256>"
          },
          "checker_version": "1",
          "mode": "offline",
          "status": "failed",
          "blocking": true,
          "code": "review_lane_workflow_raw_validator_targets_canonical_draft",
          "message": "raw lane validator still targets canonical ReviewLaneDraft",
          "report_path": "artifacts/review-lane-preflight/contract-report.v1.json",
          "checked_at_utc": "<utc-timestamp>",
          "expires_at_utc": "<utc-timestamp>"
        }
      ]
    }

Expected all-lanes infrastructure diagnostic shape:

    {
      "schema_version": 1,
      "artifact_type": "review_infrastructure_failure",
      "code": "review_infrastructure_all_lanes_failed",
      "lane_ids": ["correctness", "test-quality", "idioms-maintainability", "security-performance"],
      "execution_issues": [],
      "remote_mutations": "none"
    }

## Interfaces and Dependencies

In `scripts/scherzo_review/review_lane_contract.py`, define data and functions equivalent to:

    LANE_IDS = [
      "correctness",
      "test-quality",
      "idioms-maintainability",
      "security-performance",
    ]

    RUNNER_METADATA_FIELDS = {
      "schema_version",
      "artifact_type",
      "generated_at_utc",
      "producer",
      "lane",
      "input_refs",
      "remote_mutations",
    }

    PROVIDER_SCHEMA_ALLOWED_KEYWORDS = {
      "type",
      "description",
      "properties",
      "required",
      "additionalProperties",
      "items",
      "minLength",
      "maxLength",
      "minimum",
      "maximum",
      "minItems",
      "maxItems",
      "pattern",
    }

    def validate_provider_schema(schema: dict[str, object], schema_path: str) -> list[ContractError]: ...
    def validate_submission_shape(lane_id: str, submission: dict[str, object]) -> None: ...
    def materialize_review_lane_draft(lane_id: str, submission: dict[str, object], prepared_review_dir: Path, now_utc: str) -> dict[str, object]: ...
    def validate_canonical_artifact(artifact: dict[str, object], repo_root: Path) -> None: ...
    def run_offline_contract(workflow_path: Path, fixtures_dir: Path, output_dir: Path) -> ContractReport: ...

In `src/scherzo/structured_output_tool_spec.gleam`, expose or internally use:

    pub fn validate_provider_schema_keywords(
      schema: json_value.JsonValue,
      schema_path: String,
    ) -> Result(Nil, ToolSpecError)

This function recursively traverses objects and arrays and rejects disallowed keywords. It treats property names under `properties` as user-defined field names, not JSON Schema keywords. It treats entries under `$defs` as invalid because `$defs` itself is disallowed in provider schemas.

In `src/scherzo/review_lane_preflight_policy.gleam`, define policy types equivalent to:

    pub type ReviewLanePreflightMode {
      PreflightOff
      OfflineRequired
      LiveRequired
    }

    pub type ReviewLanePreflightPolicy {
      ReviewLanePreflightPolicy(
        mode: ReviewLanePreflightMode,
        cache_ttl_seconds: Int,
        park_on_failure: Bool,
        strict_live_model_checks: Bool,
      )
    }

In `src/scherzo/review_lane_preflight.gleam`, define types equivalent to:

    pub type ReviewLanePreflightResult {
      ReviewLanePreflightPassed(cache_key: String, checked_lanes: List(String), report_path: String)
      ReviewLanePreflightFailed(cache_key: String, code: String, message: String, report_path: String, blocking: Bool)
      ReviewLanePreflightSkipped(reason: String)
    }

    pub type ReviewLanePreflightCacheEntry {
      ReviewLanePreflightCacheEntry(
        cache_key: String,
        workflow_id: String,
        workflow_fingerprint: String,
        provider_name: String,
        model_name: String,
        tool_names: List(String),
        schema_digests: List(#(String, String)),
        checker_version: String,
        mode: String,
        status: String,
        blocking: Bool,
        code: String,
        message: String,
        report_path: String,
        checked_at_utc: String,
        expires_at_utc: String,
      )
    }

    pub fn check_workflow(
      dag: workflow_dag.WorkflowDag,
      repository_root: String,
      provider_name: String,
      model_name: String,
      policy: review_lane_preflight_policy.ReviewLanePreflightPolicy,
    ) -> ReviewLanePreflightResult

    pub fn cache_key(
      dag: workflow_dag.WorkflowDag,
      provider_name: String,
      model_name: String,
      policy_mode: String,
    ) -> String

In `src/scherzo/review_lane_live_probe.gleam`, define a `main` function for `gleam run -m scherzo/review_lane_live_probe -- --workflow <workflow> --output-dir <output-dir> --skip-if-missing-credentials` and a reusable `probe_workflow` function that returns classified live statuses. In `src/scherzo/review_lane_preflight_gate.gleam`, define a `before_claim` function that maps policy, cache, and preflight results to either `AllowClaim`, `BlockClaim(report_path, code)`, or `SkipPreflight(reason)`.

The preflight module should reuse `workflow_fingerprint` or the existing workflow canonical input code so cache invalidation follows workflow changes. The cache key must include provider schema file digests because schema content can change without changing provider or model names.

No new third-party dependencies are required for the offline path. The live path should reuse the existing Scherzo/Pi runner dependencies and provider configuration. If a new dependency appears necessary for live provider probing, stop and record a Decision Log entry explaining why existing runner APIs are insufficient.

## Open Questions and Clarifications Needed

- [CLARIFY] Confirm the credential and cost policy before enabling provider-backed contract checks in hosted CI. This plan requires offline checks in SelfCI and optional live checks when credentials are available; it does not require live checks by default.
