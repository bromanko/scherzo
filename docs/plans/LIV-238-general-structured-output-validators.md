# Generalize structured-output validators for workflow artifacts

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo workflows need structured outputs to become a generic workflow primitive, not a feature that only understands code-review lane drafts. After this plan is implemented, a workflow author can declare that an agent step must produce JSON, can choose where Scherzo captures that JSON from, and can attach generic validators that are reusable for kickoff packets, alignment proposals, review critiques, proof bundles, acceptance decisions, merge records, and future artifacts.

The observable result is that workflow YAML can declare baseline JSON admission, JSON Schema validation, and command validation without adding new Scherzo runtime variants such as `ReviewLaneDraftValidator`, `KickoffPacketValidator`, or `ProofBundleValidator`. Review-native workflows continue to run during migration, but their old `validator: review_lane_draft` syntax is replaced by generic validator declarations.

The normative source of truth for this implementation is `docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md`. This plan is reconciled to that spec and cites its requirement ids throughout. If implementation discovers that this plan and the spec disagree, the implementer must revise this ExecPlan in favor of the spec before coding further.

## Problem Framing and Constraints

Operators and workflow authors are blocked today because Scherzo can capture structured JSON, but the semantic validation hook is shaped around the current code-review workflow. In the current tree, `src/scherzo/workflow_dag.gleam` defines `StructuredOutputValidator` with a single `ReviewLaneDraftValidator` case, and `src/scherzo/structured_output.gleam` runs it by invoking `python3 scripts/scherzo-review validate-structured-output --validator review_lane_draft`. That works for review lane drafts, but it makes every new workflow artifact look like a request for another domain-specific runtime validator.

This plan solves that by splitting structured output into three generic responsibilities. First, Scherzo owns baseline JSON admission: presence, truncation, JSON parsing, object shape, simple required top-level keys, source transport checks, and safe retained payload handling. Second, JSON Schema validators handle declarative structural and value constraints. Third, command validators let a workflow run a repository-provided semantic checker through a stable stdin, stdout, stderr, exit-code, timeout, and diagnostics contract.

The main constraint is explicit: Scherzo must not add artifact-specific built-in validators. The implementation must not add or keep runtime cases named after particular artifacts, such as `ReviewLaneDraftValidator`, `KickoffPacketValidator`, `ProofBundleValidator`, or equivalent artifact-specific names. Pi tool-call structured output is also only a capture and transport mechanism. It can prove that a tool call with the configured name supplied object-valued JSON arguments, but it is not the authoritative semantic validation layer.

This plan is an implementation plan for the behavior normatively specified in `docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md`. The spec deliberately chooses the boundary that Scherzo core owns only generic source extraction, baseline admission, JSON Schema validation, command validation, retry classification, artifact persistence, diagnostics, and fingerprinting. Review-lane semantics are workflow/script concerns expressed through generic validators, not Scherzo runtime cases.

## Strategy Overview

The proportionate approach is to evolve the existing structured-output pipeline rather than replace workflow execution. The current pipeline already has the right places to parse workflow YAML, capture agent results, retry invalid required outputs, persist validated structured artifacts, and fingerprint workflow semantics. The plan keeps those pieces and replaces the review-native validator enum with a generic list of validator declarations.

At runtime, every structured output follows the same pipeline required by `SOV-PIPE-001`: Scherzo captures a candidate JSON payload from either the final assistant response or a configured Pi tool call, performs baseline admission checks, runs zero or more generic validators in workflow-declared order, and writes the structured artifact only after all validators pass. A JSON Schema validator loads a repository-relative schema and validates the admitted raw JSON through Scherzo-owned infrastructure. A command validator starts a configured command without a shell, sends the redacted admitted JSON on stdin, captures bounded diagnostics, and interprets the exit result. This split follows `SOV-PIPE-004` and `SOV-PIPE-005`.

The JSON Schema implementation is intentionally an internal helper process rather than an artifact-specific validator or a workflow-declared command. Add `scripts/scherzo-json-schema-validate`, a repository-owned Python helper that uses the `jsonschema` Python package's `Draft202012Validator`. Add `pkgs.python3Packages.jsonschema` to `devenv.nix` so the helper is available through `direnv exec .`. Gleam calls this helper through a Scherzo-owned runner module and treats its exit codes as JSON Schema pass, payload rejection, or configuration failure.

This is the right size because the workflow runner already has structured-output parsing, retry, artifact, and fingerprint concepts. A smaller change that only adds `KickoffPacketValidator` or another enum case would solve one artifact and immediately recreate the same bottleneck. A larger change that invents a separate validation service, sandbox, or new workflow language is unnecessary because JSON Schema and command validators are enough for the current and next dogfood needs.

## Alternatives Considered

The simplest rejected alternative is to add more built-in validators such as `KickoffPacketValidator` or `ProofBundleValidator`. That is intentionally not chosen. It would make Scherzo carry domain knowledge for each workflow artifact, require code changes for every new artifact type, and contradict the ticket direction.

A second rejected alternative is to trust Pi tool-call schema support as the semantic validator. Tool calls are useful for capture because they can produce structured arguments, but they are tied to the agent transport and model behavior. They do not provide Scherzo-owned diagnostics, retries, artifact persistence semantics, or a uniform validation contract across final-response and tool-call sources. Pi tool calls remain a transport source only.

A third rejected alternative is to support only command validators. Command validators are flexible, but many artifact checks are ordinary JSON shape checks that should be declarative and portable. JSON Schema validators give workflow authors a standard way to express structural constraints without writing a script, while command validators remain available for semantic checks that schemas cannot express.

A fourth rejected alternative is to embed a JSON Schema engine directly in Gleam for the first implementation. That might eventually be attractive, but the current tree has no JSON Schema dependency and the fastest low-risk route to draft 2020-12 behavior is a small repository-owned Python helper under Scherzo's control. The helper remains an internal implementation detail and can be replaced later behind the same Gleam runner interface.

A fifth rejected alternative is to implement directly from the earlier ExecPlan draft without reconciling against a normative contract. That is now rejected. `docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md` is the source of truth, and this plan must change when the spec chooses different behavior.

## Risks and Countermeasures

The first risk is accidentally preserving domain-specific validation under a different name. The countermeasure is to remove the artifact-specific validator enum from the runtime model and represent validators as generic `json_schema` and `command` declarations. Compatibility handling for `validator: review_lane_draft` must lower to a generic command validator or fail with a migration diagnostic; it must not reintroduce a `ReviewLaneDraftValidator` runtime case.

The second risk is treating command validators as sandboxed when they are not. A workflow-declared command validator is trusted workflow code, similar to an existing command step: it can run a local process, read the workspace and run root, use network access if the host allows it, and mutate files even though the contract forbids mutation. Scherzo does not sandbox command validators in this first implementation. The safety countermeasure is a clean environment by default, no shell, repository-relative path checks, bounded diagnostics, redacted stdin, explicit operator documentation, and read-only fixture commands in tests.

The third risk is accidentally passing daemon secrets to validator processes. Command validators must not inherit the daemon environment wholesale. The runner starts from a clean environment containing only `PATH`, `LANG`, `LC_ALL`, and `TMPDIR` when those are present, then adds validated workflow `env` entries, then adds Scherzo-provided `SCHERZO_*` context variables. Workflow `env` entries must not override `PATH`, `HOME`, `PWD`, or any name beginning with `SCHERZO_`. Tests must prove that variables such as `LINEAR_API_KEY`, `SCHERZO_AGENT_LINEAR_API_KEY`, and model-provider tokens are not visible to a validator unless the workflow explicitly declares them under non-reserved names.

The fourth risk is treating validator diagnostics as unbounded or unsafe. Command validators can print large output or echo secrets. The implementation must capture stdout and stderr concurrently with byte limits, redact configured secrets before storing or retrying diagnostics, and record whether diagnostics were truncated. The retained structured artifact must contain the redacted accepted payload, not raw invalid attempts.

The fifth risk is retrying failures that a new agent attempt cannot fix. Missing output, invalid JSON, JSON Schema rejection, and command exit status 1 are payload validation failures and can use `validation_retries`. Invalid workflow configuration, missing schema files, invalid schema documents, command start failures, and validator timeouts are implementation or configuration failures and should not spend an agent retry. The implementation must represent retryability explicitly instead of inferring all `structured_output_*` codes are retryable.

The sixth risk is deadlocking or orphaning validator processes. A command validator receives stdin and may write to both stdout and stderr. The runner must close stdin after one JSON document, drain stdout and stderr concurrently, enforce a wall-clock timeout, terminate the process group or the closest equivalent supported by the existing process abstraction, wait for cleanup, and retain partial redacted output in the failure diagnostic.

The seventh risk is breaking review workflows during migration. The countermeasure is a compatibility window plus an inventory step. Search the repository for `validator: review_lane_draft` and `ReviewLaneDraftValidator`, update production workflow examples and fixtures to the new `validators:` syntax, and keep parser compatibility tests for the old spelling until a separate migration decision removes legacy parsing under `SOV-COMPAT-005`.

The eighth risk is fingerprint drift. If validator declarations are not included in workflow fingerprints, Scherzo could reuse or compare workflow executions incorrectly. The implementation must add every validator field that affects validation semantics to the canonical fingerprint input, including validator order, names, types, schema paths and schema content hashes, command argv, timeout, working-directory mode, declared environment keys plus value digests, and the structured-output validator contract version required by `SOV-FP-005`. Version 1 intentionally does not hash command executable file contents, matching `SOV-FP-006`; this deferred behavior must be documented in Outcomes if implementation discovers it matters operationally.

## Progress

- [x] (2026-05-11 00:00Z) Drafted this ExecPlan from Linear issue LIV-238 and current source inspection.
- [x] (2026-05-11 00:00Z) Incorporated adversarial review findings by choosing the JSON Schema helper strategy, defining command validator trust and environment policy, fixing copyable code blocks, specifying artifact metadata, and splitting concrete implementation steps.
- [x] (2026-05-12 00:00Z) Reconciled this ExecPlan against `docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md`, added the spec conformance matrix, updated command-validator environment and fingerprint requirements, and made the spec the normative source of truth.
- [x] (2026-05-12 19:52Z) Replaced review-native runtime validator types with generic structured-output validator declarations and compatibility lowering for `validator: review_lane_draft`.
- [x] (2026-05-12 19:52Z) Added baseline admission integration, JSON Schema validation through `scripts/scherzo-json-schema-validate`, command validator execution, and retry-aware structured-output errors.
- [x] (2026-05-12 19:52Z) Migrated review-native workflow declarations and fixtures to generic `validators:` syntax while retaining legacy parser compatibility tests.
- [x] (2026-05-12 19:52Z) Updated artifact metadata, failed-step diagnostics, retry prompt diagnostics, workflow fingerprints, fixtures, and structured-output tests.
- [x] (2026-05-12 19:52Z) Ran final validation commands and updated Outcomes & Retrospective.

## Surprises & Discoveries

- Observation: The current runtime validator model has exactly one named validator, `ReviewLaneDraftValidator`, in `src/scherzo/workflow_dag.gleam`.
  Evidence: `StructuredOutputValidator` currently maps only to the string `review_lane_draft`.

- Observation: Structured-output retry behavior already exists and retries required structured-output validation failures when `validation_retries > 0`.
  Evidence: `src/scherzo/workflow_run.gleam` builds a retry prompt through `src/scherzo/workflow_structured_retry.gleam` after a structured-output failure code.

- Observation: Valid structured payloads are persisted separately from step artifacts.
  Evidence: `src/scherzo/state/artifact_store.gleam` writes `StructuredOutputArtifact` values with run, workflow, step, attempt, artifact name, format, schema required keys, and payload.

- Observation: The reviewed HTML artifact rendered important YAML and shell examples as prose and list fragments instead of copyable code blocks.
  Evidence: The review found the representative `validators:` YAML and validation gate collapsed into invalid text; this revision renders examples as indented code blocks before implementation begins.

- Observation: The retained LIV-240 workspace already contained a nearly complete implementation after Scherzo's context-recovery failure, and manual continuation could focus on verification and handoff rather than rewriting the feature.
  Evidence: `direnv exec . gleam test` passed with `1213 passed, no failures` before the final plan update.

- Observation: The post-migration inventory still finds legacy `validator: review_lane_draft` only in compatibility tests, specifications, historical plans, and migration notes, not as a production runtime type.
  Evidence: Searching `src`, `test`, `.scherzo/workflows`, `docs`, and `scripts` finds no `ReviewLaneDraftValidator` occurrence in `src/`; production workflow files use `validators:` declarations.

- Observation: The lint gates still report the existing warning inventory but no errors.
  Evidence: `direnv exec . gleam run -m glinter` and `direnv exec . gleam run -m scherzo_lint` both reported `Found 378 issues (0 errors, 292 warnings)`.

## Decision Log

- Decision: Do not add artifact-specific runtime validators.
  Rationale: The ticket explicitly requires generic mechanisms and rejects built-in validators such as `ReviewLaneDraftValidator`, `KickoffPacketValidator`, and `ProofBundleValidator`. Generic JSON Schema and command validators cover both current review artifacts and future workflow artifacts.

  Date: 2026-05-11

- Decision: Keep Scherzo-owned baseline JSON admission separate from JSON Schema validation.
  Rationale: Scherzo must consistently handle presence, truncation, transport, parsing, top-level object admission, simple required keys, redaction, retry classification, and artifact persistence before any workflow-specific validation runs.

  Date: 2026-05-11

- Decision: Use a validator list named `validators` rather than extending the singular `validator` field.
  Rationale: A list supports ordered composition of JSON Schema and command checks, avoids overloading the legacy review-native field, and makes fingerprinting deterministic.

  Date: 2026-05-11

- Decision: Treat Pi tool-call structured output as a source only.
  Rationale: Tool-call arguments are one way to transport JSON from an agent to Scherzo. The authoritative semantic validation layer is the Scherzo pipeline of baseline admission, JSON Schema validators, and command validators.

  Date: 2026-05-11

- Decision: Implement JSON Schema validation with `scripts/scherzo-json-schema-validate` using Python `jsonschema` and `Draft202012Validator`.
  Rationale: The current Gleam project has no JSON Schema dependency. A repository-owned helper gives a mature draft 2020-12 implementation, precise diagnostics, and a narrow interface while keeping workflow-visible validators generic. The helper is internal and can later be replaced without changing workflow YAML.

  Date: 2026-05-11

- Decision: Treat workflow-declared command validators as trusted code, not sandboxed code, and run them with a clean environment by default.
  Rationale: Workflow YAML already represents executable automation, but validators should not inherit daemon tokens accidentally. A clean environment plus explicit `env`, no shell, reserved variable checks, redacted stdin, and bounded diagnostics is the smallest enforceable safety policy for this implementation.

  Date: 2026-05-11

- Decision: Persist exact generic validation metadata in structured-output artifacts and keep backward-compatible decoding for older `schema_required_keys` artifacts.
  Rationale: Retained artifacts are compatibility surface. Future operators and tests need to know which baseline keys and validators accepted the payload without guessing from old review-specific fields.

  Date: 2026-05-11

- Decision: Treat `docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md` as the normative authority for implementation.
  Rationale: LIV-239 created the spec specifically to reconcile product intent, the drafted plan, current implementation, review-native needs, and future structured artifacts. When the spec and this plan disagree, the implementation must follow the spec and this plan must be amended.

  Date: 2026-05-12

- Decision: Keep legacy `validator: review_lane_draft` support as a parser compatibility shim only.
  Rationale: `SOV-COMPAT-005` makes legacy removal a separate migration decision. The implementation lowers the legacy field to a generic command validator so retained and test workflows can still parse, while new production workflow files and examples use `validators:`.

  Date: 2026-05-12

## Outcomes & Retrospective

Review incorporation completed on 2026-05-11. The plan remains unimplemented, but it resolved the review's blocking design gaps and contains copyable examples, explicit security boundaries, exact fixture and test paths, artifact metadata shape, and green-milestone validation gates.

Spec reconciliation completed on 2026-05-12. `docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md` is now the normative contract. This plan was updated to follow the spec's requirement ids, environment-variable list, artifact persistence rule, retry classification, fingerprint contract-version requirement, compatibility rules, and conformance test expectations.

Implementation completed on 2026-05-12. Scherzo now parses ordered generic `json_schema` and `command` structured-output validators, lowers legacy `validator: review_lane_draft` to a compatibility command validator, validates JSON Schema draft 2020-12 through `scripts/scherzo-json-schema-validate`, runs command validators with redacted stdin, clean environment, timeout, and bounded diagnostics, persists generic validation metadata, exposes retryable validator errors without string-prefix inference, includes validator semantics in workflow fingerprints, and migrates review-native workflow files to `validators:`. Validation passed with `direnv exec . gleam format --check src test`, `direnv exec . gleam test` reporting `1213 passed, no failures`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`; the lint commands retained the existing warning inventory and reported `0 errors`.

## Context and Orientation

Scherzo is a workflow runner written primarily in Gleam. A workflow is parsed into a directed acyclic graph, meaning a set of steps with dependency edges that must not form cycles. Agent steps ask an AI worker to perform work. Command steps run local commands. A structured output is a JSON artifact that an agent step is expected to produce so later automation can consume it reliably.

The current workflow DAG parser lives in `src/scherzo/workflow_dag.gleam`. It defines `WorkflowDag`, `WorkflowStep`, `StepKind`, and `StructuredOutputSpec`. A step with `AgentStep(prompt, structured_output)` may have a `structured_output` map in workflow YAML. A command step rejects `structured_output` entirely. Current structured outputs have `format: json`, an optional `artifact_name`, a `required` flag that defaults to true, a `source`, a simple object schema with `required` keys, a singular optional `validator`, and `validation_retries` of 0 or 1.

Structured-output sources live in `src/scherzo/structured_output_source.gleam`. The current sources are `FinalResponseSource` and `PiToolCallSource(tool_name, require_single, reject_sibling_tool_calls)`. The Pi tool-call source requires one successful tool call with the configured name and object-valued JSON arguments. That source checking is transport validation, not semantic validation of the artifact.

Runtime validation lives in `src/scherzo/structured_output.gleam`. The function `validate_agent_result` dispatches by source. Final-response validation trims the response and parses it as JSON. Tool-call validation finds matching tool calls and parses their JSON arguments. Both paths then call `validate_present_value`, which checks the simple object schema, runs the optional named validator, redacts configured secret strings from JSON strings, and returns `StructuredOutputPresent(payload_json)`.

The only current named validator is review-native. `src/scherzo/workflow_dag.gleam` parses `validator: review_lane_draft` into `ReviewLaneDraftValidator`. `src/scherzo/structured_output.gleam` turns that into the string `review_lane_draft` and invokes `python3 scripts/scherzo-review validate-structured-output --validator review_lane_draft`, sending the payload JSON to the process. That behavior should become compatibility behavior implemented through a generic command validator, not a runtime enum case.

Retry handling is in `src/scherzo/workflow_run.gleam` and `src/scherzo/workflow_structured_retry.gleam`. When an agent succeeds but structured-output validation fails, Scherzo builds a step artifact with a `structured_output` error. If the output is required and `validation_retries > 0`, it runs one retry prompt that tells the agent how to produce the expected structured output. Current parsing restricts retries to 0 or 1.

Artifacts are represented in `src/scherzo/step_artifact.gleam` and persisted through `src/scherzo/state/artifact_store.gleam`. The step artifact records whether structured output is valid, absent, or an error. A valid structured output has metadata such as artifact name, format, artifact ref, path, hash, byte count, schema status, source type, optional source tool name, and retry info. The separate structured-output artifact stores the accepted payload.

Workflow fingerprinting lives in `src/scherzo/workflow_fingerprint.gleam`. It serializes the workflow DAG to canonical JSON and includes structured-output format, artifact name, required flag, source, simple schema, singular validator, and validation retries. Generic validators must be included in the fingerprint so changes to validation semantics change the fingerprint.

The main existing tests for structured-output behavior are in `test/structured_output_test.gleam`. They cover valid JSON, missing or invalid JSON, truncation, simple required-key schema failure, optional absence, Pi tool-call source behavior, redaction, named validator behavior, artifact writing, retry metadata, and related behavior. New parser tests for this work live in `test/workflow_dag_validator_parser_test.gleam`. New fingerprint tests live in `test/workflow_fingerprint_validator_test.gleam`. New JSON Schema and command fixture files live under `test/fixtures/structured_output/`.

## Preconditions and Verified Facts

The implementation assumes the repository still contains these files and concepts. If any path has moved before implementation begins, first update this plan with the new path and equivalent symbol names.

`src/scherzo/workflow_dag.gleam` contains `StructuredOutputSpec`, `StructuredOutputFormat`, `StructuredOutputSchema`, `StructuredOutputValidator`, `read_structured_output`, `read_structured_validator`, `read_structured_schema`, and `read_structured_validation_retries`. It currently accepts only `validator: review_lane_draft` for named validators.

`src/scherzo/structured_output_source.gleam` contains `FinalResponseSource`, `PiToolCallSource`, and parser logic for `structured_output.source`. It currently requires Pi tool-call sources to have `require_single: true` and `reject_sibling_tool_calls: true`.

`src/scherzo/structured_output.gleam` contains `StructuredOutputValidation`, `StructuredOutputError`, `NamedValidatorError`, `validate_agent_result`, `validate_final_response`, `validate_tool_call_source`, `validate_schema`, `validate_named_validator`, `default_validator_runner`, and `run_scherzo_review_validator`. It currently redacts secret strings before returning a valid payload.

`src/scherzo/workflow_run.gleam` constructs step artifacts for agent successes, validates structured outputs, writes structured-output artifacts through the checkpoint writer, and triggers structured-output retry when the failure code starts with `structured_output_` except for artifact write failure.

`src/scherzo/workflow_structured_retry.gleam` builds retry diagnostics and retry prompts. It currently includes the format, artifact name, and required top-level keys in the retry prompt.

`src/scherzo/state/artifact_store.gleam` writes and reads structured-output artifacts through `write_structured_output_artifact` and `read_structured_output_artifact`.

`src/scherzo/step_artifact.gleam` defines `StructuredOutputMetadata`, `StructuredOutputOutcome`, `StructuredOutputRetryDiagnostic`, and JSON encoders and decoders for step artifacts.

`src/scherzo/workflow_fingerprint.gleam` includes structured-output fields in the canonical workflow fingerprint input.

`devenv.nix` is the dependency-management file to update for the JSON Schema helper. Add `pkgs.python3Packages.jsonschema` to its existing package list so `python3 -c "import jsonschema"` succeeds under `direnv exec .`. Do not add a JSON Schema dependency to `gleam.toml` for the first implementation.

The repository uses direnv and devenv for validation commands. From the repository root, prefer commands such as `direnv exec . gleam test`. If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the command through direnv.

## Scope Boundaries

In scope: parsing generic structured-output validator declarations from workflow YAML, representing those declarations in the workflow DAG, running baseline JSON admission, running JSON Schema validators through `scripts/scherzo-json-schema-validate`, running command validators, classifying validation failures for retry, persisting valid artifacts with generic validation metadata, redacting and truncating diagnostics, updating workflow fingerprints, migrating review-native structured-output declarations, and adding tests for all of those behaviors.

Out of scope: adding new domain-specific built-in validators, implementing kickoff or alignment workflows, implementing proof bundle workflows, changing the normative spec except when reconciliation is needed, changing Pi itself, adding non-JSON structured-output formats, supporting multiple Pi tool calls as one artifact, adding streaming validation, adding a process sandbox, enforcing read-only command validator execution at the operating-system level, or allowing command steps to declare structured outputs.

The legacy `validator` field is in migration scope only. The implementation may accept `validator: review_lane_draft` for a compatibility window, but internally it must lower to generic command validation or report a deprecation error. No new code path should depend on an artifact-specific runtime variant.

The existing simple `schema` map remains in scope as Scherzo-owned baseline admission. It is not a full JSON Schema. It should continue to mean top-level JSON object admission plus optional required top-level keys. Full JSON Schema validation belongs under the new `validators` list.

## Normative Spec Relationship

`docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md` is the normative source of truth for this implementation. This ExecPlan is intentionally more operational than the spec: it names candidate modules, tests, fixtures, helper scripts, and commit milestones. The implementer should use the spec to resolve contract questions and this plan to carry out the code changes.

The spec's required decisions are binding. Scherzo core must not contain a runtime `ReviewLaneDraftValidator` or any equivalent artifact-specific validator. Review-specific validation remains in review-local scripts and schemas that are invoked through generic JSON Schema or command validators. Pi tool-call structured output is a source extraction mechanism only. Structured artifacts are retained only after source extraction, baseline admission, and every configured validator pass.

If implementation discovers that a plan step conflicts with a requirement id in the spec, stop the implementation slice, update this plan so it conforms to the spec, record the decision in the Decision Log, and then continue. If implementation discovers that the spec is impossible to satisfy, do not silently diverge; write down the conflict, propose a spec amendment, and get review before proceeding.

## Spec Conformance Matrix

This matrix maps the normative requirement ids in `docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md` to implementation work and tests in this plan.

- `SOV-SCOPE-001`, `SOV-SCOPE-002`, `SOV-DOMAIN-001`, `SOV-DOMAIN-002`, and `SOV-DOMAIN-003`: implemented by Steps 7, 10, 25, and 36; tested by `legacy_review_validator_lowers_to_command_validator_test`, the repository inventory in Step 36, and the review workflow acceptance checks.
- `SOV-DECL-001` through `SOV-DECL-013`: implemented by Steps 5 through 11; tested by `parses_json_schema_and_command_validators_test`, `rejects_invalid_validator_declarations_test`, `legacy_review_validator_lowers_to_command_validator_test`, and parser fixture workflows under `test/fixtures/workflows/`.
- `SOV-SRC-001` through `SOV-SRC-009`: implemented by preserving and extending `src/scherzo/structured_output_source.gleam` and `src/scherzo/structured_output.gleam` in Steps 23 through 25; tested by existing source extraction coverage in `test/structured_output_test.gleam` plus final-response-only and Pi-tool-call rejection cases from review-native tests.
- `SOV-BASE-001` through `SOV-BASE-007`: implemented by Step 25; tested by existing baseline structured-output tests and `generic_validators_run_after_baseline_admission_test`.
- `SOV-PIPE-001` through `SOV-PIPE-006`: implemented by Steps 23 through 31; tested by `generic_validators_run_after_baseline_admission_test`, validator short-circuit assertions, command redacted-stdin assertions, and artifact persistence tests.
- `SOV-CMD-001` through `SOV-CMD-011`: implemented by Steps 19 through 22; tested by `test/structured_output_command_validator_test.gleam` fixture commands for exit status, environment, stdin redaction, truncation, timeout cleanup, and path rejection.
- `SOV-JS-001` through `SOV-JS-007`: implemented by Steps 12 through 18; tested by `test/structured_output_json_schema_test.gleam` and the JSON Schema helper fixture files.
- `SOV-FAIL-001` through `SOV-FAIL-007`: implemented by Steps 24 through 28; tested by `validator_failure_retryability_is_explicit_test`, retry prompt assertions in `src/scherzo/workflow_structured_retry.gleam` tests, transient agent failure tests where present, and optional-output cases.
- `SOV-ART-001` through `SOV-ART-006`: implemented by Steps 29 through 31; tested by `test/structured_output_artifact_metadata_test.gleam`, old artifact decoding fixtures, failed-attempt no-artifact assertions, and artifact write failure classification.
- `SOV-DIAG-001` through `SOV-DIAG-006`: implemented by Steps 17, 21, 25, 27, and 30; tested by JSON Schema no-payload diagnostics, command `TOPSECRET` redaction, stdout/stderr flood truncation, retry prompt length checks, and step artifact diagnostic assertions.
- `SOV-ENV-001` through `SOV-ENV-004`: implemented by Steps 8, 20, and 21; tested by command environment probe fixtures and reserved environment override parser tests.
- `SOV-FP-001` through `SOV-FP-007`: implemented by Steps 32 through 34; tested by `test/workflow_fingerprint_validator_test.gleam` including contract-version and env-value-digest cases.
- `SOV-COMPAT-001` through `SOV-COMPAT-005`: implemented by Steps 10, 35, and 36; tested by legacy parser compatibility, migrated workflow fixtures, old retained artifact decoding, and repository inventory.
- `SOV-CONF-001` through `SOV-CONF-009`: satisfied by the aggregate parser, source, baseline, JSON Schema, command, retry, artifact, fingerprint, and migration tests named above plus final validation commands in Steps 38 through 40.

## Desired Workflow YAML Syntax

The new syntax keeps `structured_output` on agent steps and adds an ordered `validators` list. The existing `schema` field is kept as baseline admission, not JSON Schema. A representative workflow step should look like this:

```text
- id: review
  kind: agent
  prompt: scripts/scherzo_review/prompts/correctness.md
  structured_output:
    artifact_name: review_lane_draft
    required: true
    source:
      type: final_response
    format: json
    schema:
      type: object
      required:
        - schema_version
        - artifact_type
        - findings
    validators:
      - name: review_lane_shape
        type: json_schema
        path: schemas/review_lane_draft.schema.json
        draft: "2020-12"
      - name: review_lane_semantics
        type: command
        argv:
          - python3
          - scripts/scherzo-review
          - validate-structured-output
          - --validator
          - review_lane_draft
        timeout_ms: 30000
        working_directory: repository
    validation_retries: 1
```

The `validators` list defaults to an empty list. Validators run in the order declared. Each validator must have `type`. Each validator may have `name`; if omitted, Scherzo assigns a stable name such as `validator_1` for diagnostics and fingerprints. Validator names must use the same conservative identifier rules as step IDs unless implementation discovers an existing validator-name helper; they should be lowercase words separated by underscores or hyphens.

A JSON Schema validator has `type: json_schema`, a repository-relative `path`, and an optional `draft`. The first implementation supports `draft: "2020-12"` through the Python `jsonschema` `Draft202012Validator`. Paths must be repository-relative, must not be empty, must not be an absolute local path such as `<absolute-local-path>`, and must not traverse outside the repository.

A command validator has `type: command`, an `argv` list, optional `timeout_ms`, optional `working_directory`, and optional `env`. The initial implementation requires `argv` rather than a shell string so workflow YAML does not introduce shell quoting ambiguity. The first `argv` element must be a non-empty executable token and is either an executable name resolved through the clean validator `PATH`, such as `python3`, or a repository-relative executable path such as `scripts/scherzo-review`. `timeout_ms` defaults to 30000 and must be positive. `working_directory` defaults to `workspace`, meaning the agent step workspace. Other allowed values are `repository` and `run_root`. `env` is a map of additional environment variables with string values; env names must match `[A-Za-z_][A-Za-z0-9_]*`, and names beginning with `SCHERZO_` plus the names `PATH`, `HOME`, and `PWD` are reserved and must be rejected.

The legacy syntax is this:

```text
structured_output:
  artifact_name: review_lane_draft
  validator: review_lane_draft
```

During migration, the parser should lower that syntax to the same internal representation as this generic declaration:

```text
structured_output:
  artifact_name: review_lane_draft
  validators:
    - name: review_lane_draft_compat
      type: command
      argv:
        - python3
        - scripts/scherzo-review
        - validate-structured-output
        - --validator
        - review_lane_draft
      timeout_ms: 30000
      working_directory: repository
```

This compatibility declaration uses a review-specific script, but Scherzo sees it only as a command validator. The runtime model must not contain an artifact-specific review validator case.

A workflow declaration that contains both `validator` and `validators` is invalid under `SOV-DECL-012`; do not try to merge the two shapes. New workflow files must use the `validators` list.

## Baseline JSON Admission Contract

Baseline admission is Scherzo-owned and runs for every present structured output before any JSON Schema or command validator. It is intentionally generic and small.

For a final-response source, Scherzo must reject a truncated capture, treat missing or blank output as absent, enforce `required`, and parse the entire trimmed response as JSON. It must reject Markdown, code fences, commentary, or transcripts by virtue of requiring the entire response to parse as one JSON value.

For a Pi tool-call source, Scherzo must find exactly one successful call with the configured name, reject sibling tool calls when configured, require JSON arguments, parse the arguments as JSON, and require object-valued arguments. These checks only prove transport validity.

For both sources, Scherzo must enforce `format: json`, require a top-level object when `schema.type: object` is configured or defaulted, and check each top-level key listed in `schema.required`. It must produce stable error codes for missing output, truncation, invalid JSON, non-object values, missing required keys, wrong tool names, failed tool calls, malformed arguments, duplicate calls, and sibling calls.

Baseline admission must not validate artifact-specific semantics. It must not know that a review draft has findings, that a proof bundle has evidence, or that a merge record has commits beyond the generic required top-level keys declared in workflow YAML. Such checks belong in JSON Schema or command validators.

After baseline admission, Scherzo should maintain two values in memory: the admitted raw JSON value and the redacted JSON value, as required by `SOV-BASE-007`. The internal JSON Schema helper may validate the raw value because it is Scherzo-owned code, not workflow-declared code. External command validators receive the redacted value so workflow-declared commands cannot receive known secrets from Scherzo through stdin. The stored artifact is the redacted value.

## JSON Schema Validator Contract

A JSON Schema validator is a generic validator declared with `type: json_schema`. It validates the admitted JSON value against a schema file. It must not contain artifact-specific Gleam code.

The first implementation uses a repository-owned Python helper at `scripts/scherzo-json-schema-validate`. The helper uses Python package `jsonschema` and `jsonschema.validators.Draft202012Validator`. Update `devenv.nix` so `direnv exec . python3 -c "import jsonschema; print(jsonschema.__version__)"` succeeds. Do not add a separate JSON Schema dependency to `gleam.toml` in this implementation.

The helper command contract is:

```text
scripts/scherzo-json-schema-validate --schema <repo-relative-schema-path> --draft 2020-12
```

The helper reads exactly one JSON document from stdin. Exit status 0 means the payload satisfies the schema. Exit status 1 means the payload is valid JSON but rejected by the schema. Exit status 2 means configuration or internal error, including missing schema file, unreadable schema file, invalid schema JSON, invalid schema document, unsupported draft, invalid payload JSON, or unexpected helper failure. The helper writes a single JSON diagnostic object to stdout for every exit status and writes only emergency traceback details to stderr on unexpected failures. The diagnostic object has this shape:

```text
{
  "status": "accepted" | "rejected" | "error",
  "code": "ok" | "json_schema_rejected" | "json_schema_config_error",
  "message": "short human-readable message",
  "instance_path": "/findings/0/title",
  "schema_path": "/properties/findings/items/required",
  "schema_file": "schemas/review_lane_draft.schema.json",
  "draft": "2020-12"
}
```

`instance_path` and `schema_path` are empty strings when not available. The helper must never echo the full payload in diagnostics.

The schema path is repository-relative and resolved from the repository root by the Gleam runner before invoking the helper. The implementation must reject empty paths, absolute local paths, paths containing traversal outside the repository, missing files, unreadable files, invalid JSON schema documents, and unsupported schema drafts with configuration error codes that do not trigger an agent retry.

The validator runs after baseline admission. A schema rejection means the agent produced JSON that does not satisfy the declared schema. That is a retryable structured-output validation failure when the output is required and retries remain. The diagnostic should identify the validator name, schema path, and concise instance path. It should not include the entire payload.

Introduce `src/scherzo/structured_output_json_schema.gleam`. It should expose a function with this effective shape:

```text
pub fn run_json_schema_validator(
  declaration: workflow_dag.JsonSchemaValidator,
  value: json_value.JsonValue,
  context: ValidatorContext,
  secrets: List(String),
) -> Result(ValidatorPass, ValidatorFailure)
```

The exact type names may follow existing project style, but the result must distinguish payload rejection from validator configuration failure. Payload rejection is retryable. Configuration failure is not retryable. Redact diagnostic messages before they reach step artifacts, retry prompts, operator summaries, or logs.

## Command Validator Contract

A command validator is a generic validator declared with `type: command`. It gives workflow authors an escape hatch for semantic checks that are hard or impossible to express in JSON Schema, while keeping Scherzo's runtime generic.

A command validator is trusted workflow code, not sandboxed code. It runs with the same trust assumption as a workflow command step: a malicious or buggy validator can inspect the workspace, use host capabilities available to its process, and mutate files even though the contract forbids mutation. Scherzo does not enforce read-only execution in the first implementation. Operators should only run workflows whose YAML and validator commands they trust.

Scherzo starts the command described by `argv` without using a shell. `argv[0]` must be non-empty. When `argv[0]` contains a path separator, resolve it as a repository-relative path and reject absolute or traversal paths. When `argv[0]` has no path separator, resolve it through the clean validator `PATH`. Set the working directory from `working_directory`, defaulting to the step workspace. Send exactly one JSON document to stdin, encoded as UTF-8 and followed by a newline, then close stdin. That JSON document is the redacted admitted payload. The command must not depend on seeing known secrets in raw form.

Scherzo starts command validators with the clean environment required by `SOV-ENV-001` through `SOV-ENV-003`. Copy only `PATH`, `LANG`, `LC_ALL`, and `TMPDIR` from the daemon environment when present. Add workflow-declared `env` entries after validating that keys match `[A-Za-z_][A-Za-z0-9_]*`, do not equal `PATH`, `HOME`, or `PWD`, and do not begin with `SCHERZO_`. Then add Scherzo-provided context variables, which always win: `SCHERZO_CONFIG_DIR`, `SCHERZO_REPO_ROOT`, `SCHERZO_RUN_ROOT`, `SCHERZO_WORKFLOW_ID`, `SCHERZO_RUN_ID`, `SCHERZO_STEP_ID`, `SCHERZO_ATTEMPT_INDEX`, `SCHERZO_WORKSPACE_PATH`, `SCHERZO_STRUCTURED_OUTPUT_ARTIFACT_NAME`, `SCHERZO_STRUCTURED_OUTPUT_FORMAT`, `SCHERZO_STRUCTURED_OUTPUT_SOURCE_TYPE`, `SCHERZO_STRUCTURED_OUTPUT_SOURCE_TOOL_NAME`, `SCHERZO_VALIDATOR_NAME`, `SCHERZO_VALIDATOR_TYPE`, and `SCHERZO_VALIDATOR_INDEX`. Optional unavailable values should be exposed as empty strings. The values may be local runtime paths or identifiers. They are operational context, not part of the persisted structured artifact.

Exit status 0 accepts the payload. Exit status 1 rejects the payload and is retryable when retries remain. Exit status 2 means validator configuration or internal error and is not retryable. Any other nonzero exit status is treated as a validator error and is not retryable unless a later spec revision deliberately expands the contract. A timeout is a validator error and is not retryable. A start failure is a validator configuration error and is not retryable.

Scherzo captures stdout and stderr separately while the command runs. It must drain both streams concurrently so a validator cannot deadlock by filling one stream while Scherzo reads the other. It redacts configured secrets from both streams, trims trailing whitespace for summaries, stores a bounded diagnostic summary in the step artifact on failure, and records whether stdout or stderr was truncated. Use the version 1 limits from `SOV-DIAG-003` and `SOV-DIAG-004`: 8192 bytes per stream for validator diagnostics and a 1000-character retry prompt summary.

On timeout, the runner must stop accepting further output, terminate the process group or closest equivalent available through the existing process abstraction, wait for the child to exit or be reaped, retain partial redacted stdout and stderr, and return `structured_output_command_timeout` with `retryable: False`. If the existing `src/scherzo/port.gleam` helpers cannot support stdin, concurrent capture, and timeout cleanup, extend them generically rather than adding validator-specific process hacks.

Command validators must be deterministic from Scherzo's perspective. They may inspect the workspace and run root if the workflow grants them that context, but they must not mutate the workspace as part of validation. Tests should use read-only fixture commands and must include an environment probe, timeout fixture, and simultaneous stdout/stderr flood fixture.

## Retry Behavior for Validator Failures

Keep `validation_retries` on `StructuredOutputSpec` and continue to allow 0 or 1 in the first implementation. A future spec can decide whether higher retry counts are allowed, but this plan does not expand retry count semantics.

A required structured output with `validation_retries: 1` should retry once when the first attempt fails because of payload shape or payload semantics. Retryable failures include missing required output, blank required output, truncated capture, invalid JSON, wrong top-level JSON type, missing baseline required keys, Pi tool-call source transport failures caused by the agent response, JSON Schema rejection, and command validator exit status 1.

A retry should not run for failures that a different agent response cannot plausibly fix. Non-retryable failures include artifact write failures, invalid workflow YAML, unsupported validator declaration, missing or invalid JSON Schema file, unsupported schema draft, command start failure, command timeout, command exit status 2, and other validator infrastructure errors.

The retry prompt should continue to identify the step ID, run root, workspace, artifact name, format, source instructions, and baseline required keys. It should add concise information about the first failing validator: validator name, validator type, failure code, and redacted diagnostic summary. It should not paste the full invalid payload into the prompt.

Retry metadata in `StructuredOutputRetryDiagnostic` should include attempt number, status, failure code, redacted message, retryable flag, optional validator name, and optional validator type. Update the JSON decoder in `src/scherzo/step_artifact.gleam` to read older artifacts that lack the new fields by defaulting `retryable` from the current code classification and defaulting validator fields to absent.

## Artifact Persistence Behavior

Only accepted structured outputs are persisted as structured-output artifacts. A payload is accepted only after baseline admission and every declared validator passes. Invalid attempts are represented in the step artifact as errors and retry diagnostics, but their raw payloads are not written as retained structured artifacts.

The persisted payload must be the redacted JSON value. This preserves the current safety behavior in `src/scherzo/structured_output.gleam`, where secret strings are redacted before `StructuredOutputPresent(payload_json)` is returned. Command validators receive the redacted value too, so persisted artifacts and command input match for external validators.

New structured-output artifact JSON written by `src/scherzo/state/artifact_store.gleam` must use this shape in addition to any existing run, workflow, step, attempt, path, hash, or storage bookkeeping fields that already exist:

```text
{
  "artifact_name": "review_lane_draft",
  "format": "json",
  "validation": {
    "baseline": {
      "schema_type": "object",
      "required_keys": ["schema_version", "artifact_type", "findings"]
    },
    "validators": [
      {
        "name": "review_lane_shape",
        "type": "json_schema",
        "status": "passed",
        "schema_path": "schemas/review_lane_draft.schema.json",
        "schema_sha256": "<sha256-hex>",
        "draft": "2020-12"
      },
      {
        "name": "review_lane_semantics",
        "type": "command",
        "status": "passed",
        "argv": ["python3", "scripts/scherzo-review", "validate-structured-output", "--validator", "review_lane_draft"],
        "argv_sha256": "<sha256-hex>",
        "timeout_ms": 30000,
        "working_directory": "repository",
        "env_keys": []
      }
    ]
  },
  "payload": {
    "schema_version": 1,
    "artifact_type": "review_lane_draft"
  }
}
```

`schema_sha256` is the SHA-256 of the schema file contents used for validation. `argv_sha256` is the SHA-256 of the canonical JSON representation of the argv list. `env_keys` contains sorted environment variable names only, not values. Do not persist command stdout or stderr on success. During one compatibility release, it is acceptable to also write the old top-level `schema_required_keys` field if existing readers still need it, but the decoder must treat `validation.baseline.required_keys` as authoritative when both are present.

Backward-compatible decoding is required. When reading an old artifact that lacks `validation` and has `schema_required_keys`, decode it as baseline validation with `schema_type: "object"`, `required_keys` from `schema_required_keys`, and an empty validator list. When reading an old artifact that lacks both fields, decode baseline required keys as an empty list and validators as an empty list.

Update `src/scherzo/step_artifact.gleam` so `StructuredOutputMetadata` reports generic validation status without duplicating the full retained artifact. A valid step artifact should include artifact name, format, ref, hash, bytes, source type, source tool name, retry information, baseline required keys, and validator summaries with name, type, and status. A failed step artifact should include error code, redacted message, retryable flag, optional validator name and type, stdout/stderr truncation booleans for command failures, and a bounded redacted diagnostic summary. Maintain backward-compatible decoders for old step artifact JSON that lacks validator metadata.

If writing the structured-output artifact fails after validation passes, keep the current behavior: fail the step with `structured_output_artifact_write_failed` and do not retry the agent. A new agent response does not fix storage failure.

## Diagnostics Capture, Truncation, and Redaction

Diagnostics are any human-readable details Scherzo records when validation fails. They include parser messages, baseline admission messages, JSON Schema validation messages, command stdout, command stderr, timeout details, and process start errors.

All diagnostics that can reach a step artifact, retry prompt, retained artifact, operator summary, or logs must pass through the same redaction function used for structured-output strings. Use the existing redaction behavior in `src/scherzo/log.gleam` through the same call style already used by `src/scherzo/structured_output.gleam` and `src/scherzo/workflow_structured_retry.gleam`.

Diagnostics must be bounded. For command validators, capture at most 8192 bytes of stdout and 8192 bytes of stderr for persisted diagnostics. Record booleans or equivalent metadata indicating truncation for each stream. For retry prompts, include at most 1000 characters of diagnostic summary and prefer the first useful error over a bulk transcript. For JSON Schema validators, prefer the helper's concise error message and instance path. Do not include full payloads in diagnostics.

Error codes should be stable and generic. Use these names unless implementation discovers an existing naming convention that requires a mechanical adjustment across tests: `structured_output_missing`, `structured_output_truncated`, `structured_output_invalid_json`, `structured_output_schema_invalid` for baseline simple schema failures, `structured_output_json_schema_rejected`, `structured_output_json_schema_config_error`, `structured_output_command_rejected`, `structured_output_command_config_error`, `structured_output_command_timeout`, and the existing Pi tool-call transport codes. Tests must lock whichever names are chosen.

## Workflow Fingerprint Changes

Update `src/scherzo/workflow_fingerprint.gleam` to include the new validator list in the canonical workflow JSON. Validator order is semantic and must be preserved. Each validator entry should include its stable name, type, and fields that affect validation behavior.

For JSON Schema validators, include the repository-relative schema path, requested draft, and a SHA-256 hash of the schema file contents when the schema file is available during fingerprinting for execution. If the current pure DAG fingerprint cannot read files, include the path and requested draft in the pure DAG fingerprint and include the content hash in the execution fingerprint. This mirrors the distinction already present between pure DAG fingerprinting and execution fingerprinting.

For command validators, include argv exactly as a list, timeout, working-directory mode, environment keys, and SHA-256 digests of configured environment values. Do not persist environment values in cleartext. Include a structured-output validator contract version, currently `1`, in the fingerprint input. Do not hash the contents of external commands in the first implementation because command steps currently fingerprint command text rather than every executable they might call; `SOV-FP-006` intentionally defers that behavior.

Legacy `validator: review_lane_draft` and the explicit compatibility command declaration should canonicalize to the same generic validator representation during the compatibility window if practical. If exact equivalence is too invasive, accept a one-time fingerprint change during migration and document it in the Decision Log before implementation continues.

Add fingerprint tests showing that the fingerprint changes when a validator is added, removed, reordered, renamed, or edited; when a JSON Schema path or content hash changes; and when command argv or timeout changes. Also test that unrelated formatting changes in YAML do not change the parsed canonical fingerprint.

## Backwards Compatibility and Migration Strategy

The migration should happen in three small stages and must follow `SOV-COMPAT-001` through `SOV-COMPAT-005`. Declarations containing both the legacy `validator` field and the generic `validators` field must be rejected as ambiguous.

First, add generic validator declarations and runners while still accepting the legacy singular `validator` field. Internally, the legacy field lowers to a generic command validator with a deprecation diagnostic. At this stage, old workflows continue to run, and new workflows can use `validators:`.

Second, update review workflow YAML and fixtures to use `validators:`. The review migration should not rely on a built-in `ReviewLaneDraftValidator`. If the existing review validation script remains useful, call it through a command validator. If a JSON Schema file for review lane drafts is available or added during implementation, prefer a JSON Schema validator for shape and keep the script only for checks that cannot be expressed as JSON Schema.

Third, after the follow-up normative spec is accepted and downstream workflow files have migrated, remove the compatibility parser for the legacy `validator` field. That removal is not part of the first implementation unless the team explicitly decides there are no existing workflow files using the legacy field.

Before changing review workflow files, inventory current usage from the repository root:

```text
rg "validator: review_lane_draft|ReviewLaneDraftValidator|review_lane_draft" src test .scherzo/workflows docs scripts
```

Expected results before implementation include `src/scherzo/workflow_dag.gleam`, `src/scherzo/structured_output.gleam`, structured-output tests, and review workflow fixtures or examples. The implementer must update this paragraph with the exact files found if the inventory differs. Acceptance for migration is that no production workflow example uses `validator: review_lane_draft`, parser compatibility tests still cover the old spelling, and no runtime type named `ReviewLaneDraftValidator` remains in `src/`.

Compatibility tests must cover both old and new syntax during the compatibility window. Old syntax should either produce the same validation behavior as the lowered command declaration or produce a clear deprecation error if compatibility is intentionally disabled. New syntax should be the only syntax shown in examples and docs created by this implementation.

## Milestones

Milestone 1 establishes the generic workflow model and parser. At the end, workflow YAML can parse `validators:` declarations into generic types, invalid declarations produce stable parser errors, and the legacy `validator: review_lane_draft` path lowers to a generic command validator for compatibility. This comes first because all later runtime work depends on the data model.

Milestone 2 proves the chosen JSON Schema engine. At the end, `devenv.nix` provides Python `jsonschema`, `scripts/scherzo-json-schema-validate` accepts draft 2020-12 schemas, rejects invalid payloads with an instance path, rejects invalid schema files as configuration errors, and runs through `src/scherzo/structured_output_json_schema.gleam` under `direnv exec . gleam test`. This milestone retires the largest dependency uncertainty before runtime integration.

Milestone 3 implements the command validator runner. At the end, command validators run without a shell, receive redacted JSON on stdin, use the clean environment policy, classify exit statuses, bound and redact diagnostics, and clean up on timeout without deadlocking on stdout or stderr.

Milestone 4 connects generic validators to workflow execution, retries, and artifact persistence. At the end, agent step execution uses the generic pipeline, retry prompts include validator diagnostics, valid artifacts store generic validation metadata, old artifacts still decode, and non-retryable validator configuration failures do not spend an agent retry.

Milestone 5 updates workflow fingerprints and migrates review-native usage. At the end, validator declarations affect fingerprints, review fixtures and examples use `validators:`, and legacy syntax remains only as a documented compatibility shim.

Milestone 6 completes validation and documentation handoff. At the end, parser, JSON Schema, command, retry, artifact persistence, fingerprinting, and review compatibility tests pass, lint gates pass, this plan's living sections are updated, and any details requiring normative spec follow-up are listed in Outcomes & Retrospective.

## Plan of Work

In `src/scherzo/workflow_dag.gleam`, replace the artifact-specific `StructuredOutputValidator` enum with generic declaration types. A concrete shape is `StructuredOutputValidator` with cases `JsonSchemaValidator(name: String, path: String, draft: Option(String))` and `CommandValidator(name: String, argv: List(String), timeout_ms: Int, working_directory: ValidatorWorkingDirectory, env: List(#(String, String)))`. If project style prefers records, use records, but keep the generic cases and avoid artifact-specific names.

In the same file, add parser functions for `structured_output.validators`. The parser should reject non-list values, missing validator `type`, unknown types, invalid names, invalid paths, empty argv, non-string argv entries, invalid timeout values, invalid working-directory values, reserved environment variable names, and environment maps with non-string values. Keep `structured_output.schema` as baseline admission. Change `read_structured_validator` into compatibility parsing for the legacy singular `validator` field and lower `review_lane_draft` to a generic command validator. Reject any other legacy validator name.

Add `scripts/scherzo-json-schema-validate` and `src/scherzo/structured_output_json_schema.gleam`. Update `devenv.nix` with `pkgs.python3Packages.jsonschema`. The helper validates schema files with `Draft202012Validator.check_schema`, validates payloads with `Draft202012Validator(schema).iter_errors(payload)`, reports the first sorted error by instance path, and exits with the helper contract defined above.

Add `src/scherzo/structured_output_command_validator.gleam`. Reuse existing `src/scherzo/port.gleam` process helpers if they support stdin, concurrent stdout/stderr capture, timeouts, and environment control. If they do not, extend `src/scherzo/port.gleam` generically. Do not add review-specific process helpers.

In `src/scherzo/structured_output.gleam`, split baseline admission from validator execution. Rename or wrap the current simple `validate_schema` as baseline admission. Replace `NamedValidatorError` with generic validator result types that include validator name, validator type, code, message, retryable flag, stdout/stderr truncation metadata where relevant, and a bounded redacted diagnostic summary. Keep `validate_agent_result` as the public entry point if practical so callers in `src/scherzo/workflow_run.gleam` need minimal changes.

In `src/scherzo/workflow_run.gleam`, replace retry classification that checks whether failure codes start with `structured_output_` with classification from the structured-output error itself. Artifact write failures stay non-retryable. Payload validation failures retry when required and retries remain. Configuration and infrastructure failures do not retry.

In `src/scherzo/workflow_structured_retry.gleam`, update retry prompt construction to include the first failing validator name and type when available. Preserve source-specific instructions for final response and Pi tool-call sources. Keep the prompt concise and avoid full payloads.

In `src/scherzo/state/artifact_store.gleam`, update `StructuredOutputArtifact` and its encoder and decoder for the exact generic validation metadata described in Artifact Persistence Behavior. Preserve decoding of older artifacts that only have `schema_required_keys`. Do not break existing retained artifacts.

In `src/scherzo/step_artifact.gleam`, update structured-output outcome metadata and retry diagnostics with validator summaries, retryable flags, and command truncation booleans. Maintain backward-compatible decoders for old step artifact JSON that lacks validator metadata.

In `src/scherzo/workflow_fingerprint.gleam`, update structured-output canonical JSON. Replace singular validator serialization with ordered validator-list serialization. Add schema content hashing in the execution fingerprint path for JSON Schema validators if file access is available there; otherwise introduce the smallest context object needed to provide schema hashes during execution fingerprinting.

In tests, extend `test/structured_output_test.gleam` for baseline, pipeline, retryability, and integrated validation behavior. Create `test/workflow_dag_validator_parser_test.gleam` for parser coverage if no narrower existing parser test file is clearly better. Create `test/structured_output_json_schema_test.gleam` and `test/structured_output_command_validator_test.gleam` for the two runner modules. Create `test/structured_output_artifact_metadata_test.gleam` for artifact and step metadata compatibility if existing artifact tests cannot house the new cases cleanly. Create `test/workflow_fingerprint_validator_test.gleam` for fingerprint coverage. Add fixtures under `test/fixtures/structured_output/` and workflow YAML fixtures under `test/fixtures/workflows/` as named in Concrete Steps.

## Concrete Steps

Step 0. Read `docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md` from top to bottom. Keep the spec open while implementing. If any instruction in this plan conflicts with a requirement id in the spec, update this plan to match the spec before coding.

Step 1. From the repository root, inspect source-control status with the workspace driver:

```text
$SCHERZO_WORKSPACE_DRIVER status --human
```

Expect a clean tree before implementation begins except for this ExecPlan artifact if it has not yet been committed.

Step 2. From the repository root, run the baseline test suite:

```text
direnv exec . gleam test
```

Expect all existing tests to pass. If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry through direnv.

Step 3. From the repository root, run the baseline lint gates as separate commands:

```text
direnv exec . gleam format --check src test
direnv exec . gleam run -m glinter
direnv exec . gleam run -m scherzo_lint
```

Expect each command to exit 0. If a lint command reports pre-existing warnings unrelated to this work, do not broaden the refactor; avoid adding new warnings and record the unrelated baseline in Surprises & Discoveries.

Step 4. Run the migration inventory command:

```text
rg "validator: review_lane_draft|ReviewLaneDraftValidator|review_lane_draft" src test .scherzo/workflows docs scripts
```

Record the exact files found in Surprises & Discoveries if they differ from the current expectations in Backwards Compatibility and Migration Strategy.

Step 5. Create `test/workflow_dag_validator_parser_test.gleam`. Add `parses_json_schema_and_command_validators_test`, which parses a workflow YAML string containing the representative `validators:` list from Desired Workflow YAML Syntax and asserts that the agent step's `StructuredOutputSpec` has two validators in order: `JsonSchemaValidator(name: "review_lane_shape", path: "schemas/review_lane_draft.schema.json", draft: Some("2020-12"))` and `CommandValidator(name: "review_lane_semantics", argv: ["python3", "scripts/scherzo-review", "validate-structured-output", "--validator", "review_lane_draft"], timeout_ms: 30000, working_directory: ValidatorInRepository or the project-equivalent repository mode, env: [])`. Also add parser assertions for spec defaults from `SOV-DECL-002` through `SOV-DECL-011`, including default validator names when omitted.

Step 6. Run `direnv exec . gleam test`. The new parser test should fail before implementation because `JsonSchemaValidator`, `CommandValidator`, or the `validators` field is not defined or parsed. If it passes without code changes, inspect whether the test accidentally asserts the old singular validator behavior.

Step 7. In `src/scherzo/workflow_dag.gleam`, add the generic validator types and `ValidatorWorkingDirectory` type. Replace the singular optional validator field in `StructuredOutputSpec` with `validators: List(StructuredOutputValidator)`. If retaining a compatibility field temporarily is mechanically easier, keep it private to parsing and do not expose artifact-specific runtime cases.

Step 8. In `src/scherzo/workflow_dag.gleam`, add parser helpers for validator lists, names, JSON Schema paths, command argv, timeout, working directory, and environment. Reject empty command executable tokens, invalid env key syntax, reserved env keys `PATH`, `HOME`, `PWD`, and keys beginning with `SCHERZO_`. Reject duplicate validator names after generated names are assigned. Reject declarations that contain both `validator` and `validators`. Keep `schema` parsing unchanged as baseline admission.

Step 9. In `test/workflow_dag_validator_parser_test.gleam`, add `rejects_invalid_validator_declarations_test` with table-style cases for non-list `validators`, missing `type`, unknown `type`, empty JSON Schema `path`, traversal path, empty command `argv`, empty command executable token, non-string argv entry, non-positive `timeout_ms`, invalid `working_directory`, non-string env value, invalid env key syntax, reserved env key, duplicate validator names, and simultaneous `validator` plus `validators`. Assert stable parser error messages or codes that include the invalid field name.

Step 10. In `test/workflow_dag_validator_parser_test.gleam`, add `legacy_review_validator_lowers_to_command_validator_test`. It parses `validator: review_lane_draft` and asserts that the parsed internal representation contains exactly one generic command validator named `review_lane_draft_compat` with argv `python3 scripts/scherzo-review validate-structured-output --validator review_lane_draft`, `working_directory: repository`, and no `ReviewLaneDraftValidator` constructor.

Step 11. Run `direnv exec . gleam test` and make the parser tests pass by completing parser implementation. At this point, commit a green parser milestone with a message like `Add generic structured-output validator parsing`.

Step 12. Add `test/fixtures/structured_output/review_lane_draft.schema.json`. It should be a draft 2020-12 schema requiring `schema_version`, `artifact_type`, and `findings`; requiring `artifact_type` to equal `review_lane_draft`; and requiring `findings` to be an array.

Step 13. Add `test/fixtures/structured_output/invalid_schema.schema.json` with a deliberately invalid draft 2020-12 schema, such as a `type` value that the library rejects. Add `test/fixtures/structured_output/review_lane_payload_valid.json` and `test/fixtures/structured_output/review_lane_payload_invalid.json` with one valid payload and one payload whose `findings` value violates the schema.

Step 14. Create `test/structured_output_json_schema_test.gleam`. Add `json_schema_accepts_valid_payload_test`, `json_schema_rejects_invalid_payload_with_instance_path_test`, `json_schema_missing_file_is_non_retryable_config_error_test`, `json_schema_invalid_schema_is_non_retryable_config_error_test`, and `json_schema_rejects_absolute_or_traversal_paths_test`. These tests call `structured_output_json_schema.run_json_schema_validator` through the planned Gleam API. Before implementation, `direnv exec . gleam test` should fail to compile because `structured_output_json_schema` does not exist.

Step 15. Update `devenv.nix` to include `pkgs.python3Packages.jsonschema`. From the repository root, run:

```text
direnv exec . python3 -c "import jsonschema; print(jsonschema.__version__)"
```

Expect the command to print a version and exit 0.

Step 16. Add executable helper `scripts/scherzo-json-schema-validate`. Implement the helper contract from JSON Schema Validator Contract using `Draft202012Validator`. Make it read payload JSON from stdin, validate the schema document first, then validate the payload, and always write one diagnostic JSON object to stdout for expected pass, reject, and configuration-error cases.

Step 17. Add `src/scherzo/structured_output_json_schema.gleam`. It should validate repository-relative schema paths, call the helper through the existing process abstraction, parse the helper diagnostic JSON, redact messages, and map helper exit 0 to `ValidatorPass`, exit 1 to retryable `structured_output_json_schema_rejected`, and exit 2 or process failure to non-retryable `structured_output_json_schema_config_error`.

Step 18. Run `direnv exec . gleam test`. The JSON Schema tests should pass. If the helper cannot provide an instance path or cannot validate draft 2020-12, stop, record the discovery in this plan, and revise the JSON Schema strategy before moving to command validators. Commit a green JSON Schema milestone with a message like `Run JSON Schema structured-output validators`.

Step 19. Add command fixture scripts under `test/fixtures/structured_output/`: `command_validator_accept.py` reads stdin and exits 0; `command_validator_reject.py` writes `reject secret=TOPSECRET` to stderr and exits 1; `command_validator_exit_2.py` exits 2; `command_validator_env_probe.py` writes selected environment keys as JSON; `command_validator_stream_flood.py` writes more than 8192 bytes to both stdout and stderr; and `command_validator_sleep.py` sleeps longer than the test timeout. Each fixture must be read-only and deterministic.

Step 20. Create `test/structured_output_command_validator_test.gleam`. Add `command_validator_exit_0_accepts_test`, `command_validator_exit_1_is_retryable_test`, `command_validator_exit_2_is_non_retryable_test`, `command_validator_other_nonzero_is_non_retryable_test`, `command_validator_receives_redacted_stdin_test`, `command_validator_uses_clean_environment_test`, `command_validator_exposes_scherzo_context_env_test`, `command_validator_rejects_reserved_env_overrides_test`, `command_validator_truncates_stdout_and_stderr_without_deadlock_test`, `command_validator_timeout_cleans_up_process_test`, and `command_validator_rejects_absolute_or_traversal_argv_path_test`. The environment test must assert the `SOV-ENV-003` variables, including `SCHERZO_REPO_ROOT`, `SCHERZO_CONFIG_DIR`, `SCHERZO_VALIDATOR_TYPE`, and `SCHERZO_STRUCTURED_OUTPUT_SOURCE_TYPE`. Before implementation, `direnv exec . gleam test` should fail to compile because the command validator runner does not exist.

Step 21. Add `src/scherzo/structured_output_command_validator.gleam`. Implement argv validation, working-directory resolution, clean environment assembly, reserved environment key rejection, Scherzo context variables, redacted stdin, exit-code classification, redacted bounded diagnostics, and timeout handling. Extend `src/scherzo/port.gleam` generically if the existing helper cannot close stdin, drain both streams concurrently, and terminate on timeout.

Step 22. Run `direnv exec . gleam test`. The command validator tests should pass. Inspect failure diagnostics to confirm `TOPSECRET` is redacted, inherited daemon token names are absent unless explicitly declared under allowed names, and stdout/stderr truncation booleans are true for the flood fixture. Commit a green command milestone with a message like `Run command structured-output validators`.

Step 23. In `test/structured_output_test.gleam`, add `generic_validators_run_after_baseline_admission_test`. It passes a valid final-response JSON object with both validators and asserts that baseline admission runs first, the JSON Schema validator sees the raw admitted value, the command validator receives the redacted JSON string on stdin, and the final result is `StructuredOutputPresent` with the redacted payload.

Step 24. In `test/structured_output_test.gleam`, add `validator_failure_retryability_is_explicit_test`. Assert that missing required output, JSON Schema rejection, and command exit 1 return retryable errors, while missing schema file, invalid schema document, command start failure, command timeout, and command exit 2 return non-retryable errors. Before implementation, this should fail because errors do not expose a retryable flag.

Step 25. In `src/scherzo/structured_output.gleam`, split baseline admission from validator execution and replace named-validator code with the generic validator pipeline. Add helper functions equivalent to `error_code`, `error_message`, and `error_retryable` so callers do not parse strings.

Step 26. In `src/scherzo/workflow_run.gleam`, use the explicit retryable flag from structured-output errors rather than checking whether error codes start with `structured_output_`. Keep artifact write failures non-retryable.

Step 27. In `src/scherzo/workflow_structured_retry.gleam`, include validator name, validator type, failure code, retryable flag, and a redacted diagnostic summary in retry prompts. Do not include full payloads.

Step 28. Run `direnv exec . gleam test`. The pipeline and retry tests should pass. Commit a green pipeline milestone with a message like `Classify generic structured-output validation failures`.

Step 29. Create or extend `test/structured_output_artifact_metadata_test.gleam`. Add `accepted_artifact_persists_generic_validation_metadata_test`, `old_schema_required_keys_artifact_decodes_test`, `valid_step_metadata_includes_validator_summaries_test`, and `failed_step_metadata_includes_retryable_validator_error_test`. Use the exact JSON shape in Artifact Persistence Behavior as the expected new artifact shape.

Step 30. In `src/scherzo/state/artifact_store.gleam`, update `StructuredOutputArtifact` and its encoder and decoder for `validation.baseline` and `validation.validators`. Keep old `schema_required_keys` decoding. In `src/scherzo/step_artifact.gleam`, update structured-output metadata and retry diagnostics with validator summaries, retryable flags, and truncation booleans.

Step 31. Run `direnv exec . gleam test`. Artifact metadata tests should pass, and older retained artifact fixture JSON should still decode. Commit a green artifact milestone with a message like `Persist generic structured-output validation metadata`.

Step 32. Create `test/workflow_fingerprint_validator_test.gleam`. Add tests named `fingerprint_changes_when_validator_added_test`, `fingerprint_changes_when_validator_order_changes_test`, `fingerprint_changes_when_json_schema_path_changes_test`, `fingerprint_changes_when_json_schema_content_hash_changes_test`, `fingerprint_changes_when_command_argv_changes_test`, `fingerprint_changes_when_command_timeout_changes_test`, `fingerprint_changes_when_command_env_value_digest_changes_test`, `fingerprint_includes_structured_output_contract_version_test`, and `legacy_review_validator_fingerprints_like_compat_command_test` when practical.

Step 33. In `src/scherzo/workflow_fingerprint.gleam`, serialize ordered validator lists into the canonical workflow JSON. Add schema content hashing in the execution fingerprint path. Include command environment keys and SHA-256 value digests, not cleartext values. Include structured-output validator contract version `1`. Preserve deterministic sorting for environment maps and preserve validator order.

Step 34. Run `direnv exec . gleam test`. Fingerprint tests should pass. Commit a green fingerprint milestone with a message like `Fingerprint generic structured-output validators`.

Step 35. Create `test/fixtures/workflows/structured_output_generic_validators.yaml` with a minimal agent step using final-response structured output, baseline required keys, `review_lane_draft.schema.json`, and `command_validator_accept.py`. Create `test/fixtures/workflows/structured_output_legacy_review_validator.yaml` with the old `validator: review_lane_draft` syntax for parser compatibility only.

Step 36. Update review workflow YAML, fixtures, or examples found by the inventory command so production examples use `validators:`. Do not remove the legacy parser path yet. Search again with:

```text
rg "validator: review_lane_draft|ReviewLaneDraftValidator" src test .scherzo/workflows docs scripts
```

Expect no `ReviewLaneDraftValidator` runtime type in `src/`, no production workflow example using `validator: review_lane_draft`, and only compatibility tests or migration notes using the legacy spelling.

Step 37. Run `direnv exec . gleam test`. Review compatibility and migrated workflow tests should pass. Commit a green migration milestone with a message like `Migrate review structured-output validators`.

Step 38. Run final formatting and tests from the repository root:

```text
direnv exec . gleam format --check src test
direnv exec . gleam test
```

Expect formatting to pass and all tests to pass.

Step 39. Run final lint gates from the repository root:

```text
direnv exec . gleam run -m glinter
direnv exec . gleam run -m scherzo_lint
```

Expect both commands to exit 0. If they report warnings that predate this work, record the baseline and fix only warnings introduced by this work unless the fix is local and mechanical.

Step 40. Validate the plan artifact after updating Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective. Also inspect `docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md` links and referenced paths by reading them in the repository tree:

```text
scripts/scherzo-execplan validate docs/plans/LIV-238-general-structured-output-validators.html
```

Expect the validator to report `VALIDATION=ok` or the project-equivalent success message. Commit the final green milestone with a message like `Complete generic structured-output validator plan implementation`.

## Testing and Falsifiability

Baseline admission tests must preserve current behavior. Existing tests in `test/structured_output_test.gleam` should still prove valid JSON is accepted, required missing output fails, optional missing output succeeds, invalid JSON fails, truncated capture fails, non-object JSON fails when object admission is configured, missing baseline required keys fail, Pi tool-call sources reject missing, wrong, failed, malformed, multiple, and sibling calls, and secret strings are redacted from accepted payloads.

Parser tests in `test/workflow_dag_validator_parser_test.gleam` prove the YAML contract. `parses_json_schema_and_command_validators_test` fails before implementation because the generic constructors and fields are absent, then passes when ordered validators parse. `rejects_invalid_validator_declarations_test` proves invalid names, paths, argv, timeout, working directory, and env entries fail at parse time. `legacy_review_validator_lowers_to_command_validator_test` proves compatibility without a `ReviewLaneDraftValidator` runtime case.

JSON Schema tests in `test/structured_output_json_schema_test.gleam` prove the chosen engine. `json_schema_accepts_valid_payload_test` passes a valid review lane payload and expects `Ok(ValidatorPass)`. `json_schema_rejects_invalid_payload_with_instance_path_test` passes a payload with invalid `findings` and expects `Err` with code `structured_output_json_schema_rejected`, `retryable: True`, validator name `review_lane_shape`, and a non-empty instance path. `json_schema_missing_file_is_non_retryable_config_error_test` and `json_schema_invalid_schema_is_non_retryable_config_error_test` expect code `structured_output_json_schema_config_error` and `retryable: False`. `json_schema_rejects_absolute_or_traversal_paths_test` uses `<absolute-local-path>` as a placeholder in the assertion description and repository-relative traversal strings in test data; it expects path validation to fail before helper execution.

Command tests in `test/structured_output_command_validator_test.gleam` prove the trust and process contract. `command_validator_exit_1_is_retryable_test` calls the runner with `command_validator_reject.py` and expects code `structured_output_command_rejected`, `retryable: True`, and redacted diagnostics. `command_validator_uses_clean_environment_test` sets token-like variables in the parent test process, runs `command_validator_env_probe.py`, and asserts that undeclared inherited secrets are absent while Scherzo context variables are present. `command_validator_rejects_reserved_env_overrides_test` asserts parser or runner rejection for `PATH`, `HOME`, `PWD`, and `SCHERZO_RUN_ID`. `command_validator_truncates_stdout_and_stderr_without_deadlock_test` runs `command_validator_stream_flood.py` and expects both truncation booleans. `command_validator_timeout_cleans_up_process_test` runs `command_validator_sleep.py` with a short timeout and expects `structured_output_command_timeout`, `retryable: False`, partial redacted diagnostics, and no orphaned process according to the available process abstraction.

Pipeline tests in `test/structured_output_test.gleam` prove validators are actually used by `validate_agent_result`. `generic_validators_run_after_baseline_admission_test` expects baseline required-key failure to stop before validators, JSON Schema rejection to stop before command validators, and command validators to run only after schema pass. `validator_failure_retryability_is_explicit_test` proves retryable and non-retryable failures without string-prefix inference.

Artifact tests in `test/structured_output_artifact_metadata_test.gleam` prove the retained JSON compatibility surface. `accepted_artifact_persists_generic_validation_metadata_test` compares the encoded artifact to the shape in Artifact Persistence Behavior. `old_schema_required_keys_artifact_decodes_test` loads an old fixture with `schema_required_keys` and expects `validation.baseline.required_keys` with an empty validator list. `failed_step_metadata_includes_retryable_validator_error_test` expects a step artifact error with validator name, type, code, retryable flag, and bounded diagnostic summary.

Fingerprint tests in `test/workflow_fingerprint_validator_test.gleam` prove semantic changes affect fingerprints. The tests must compare fingerprints before and after adding a validator, reordering validators, changing a JSON Schema path, changing schema file contents, changing command argv, changing timeout, and changing env entries. Formatting-only YAML changes must not change the fingerprint.

The plan is falsified if any `SOV-CONF-*` test category cannot be satisfied, if JSON Schema draft 2020-12 cannot be run under `direnv exec .`, if command validators inherit undeclared token variables, if stdout/stderr flood tests deadlock, if validator configuration failures spend an agent retry, if old artifact JSON no longer decodes, if structured artifacts are persisted before validators pass, if `src/` still contains a domain-specific runtime validator such as `ReviewLaneDraftValidator`, or if fingerprints ignore validator declarations.

## Validation and Acceptance

After implementation, from the repository root, run:

```text
direnv exec . gleam format --check src test
```

Expect exit 0 with no formatting changes required.

Then run:

```text
direnv exec . gleam test
```

Expect all tests to pass, including `test/workflow_dag_validator_parser_test.gleam`, `test/structured_output_json_schema_test.gleam`, `test/structured_output_command_validator_test.gleam`, `test/structured_output_artifact_metadata_test.gleam`, `test/workflow_fingerprint_validator_test.gleam`, and existing `test/structured_output_test.gleam` coverage.

Then run:

```text
direnv exec . gleam run -m glinter
```

Expect exit 0. Treat warnings as a ratchet inventory and do not add new production warnings.

Then run:

```text
direnv exec . gleam run -m scherzo_lint
```

Expect exit 0 and no new Scherzo-specific lint violations.

Manual acceptance can be demonstrated with `test/fixtures/workflows/structured_output_generic_validators.yaml`. A valid JSON response should produce a step artifact whose structured output status is valid and whose structured artifact ref can be read only after every validator passes. An invalid JSON response should fail with `structured_output_invalid_json` or the final chosen equivalent. A JSON Schema violation should produce a validator diagnostic and retry once if required. A command validator exit 1 should produce a redacted diagnostic and retry once if required. A missing schema file should fail without retry. A deliberately failing command validator must not leave a retained structured artifact for that failed attempt.

Review workflow acceptance is that review-native structured outputs no longer require `ReviewLaneDraftValidator` in Scherzo's runtime types. The migrated workflow uses `validators:` and still accepts a valid review lane draft and rejects an invalid one through generic mechanisms. A repository check for `ReviewLaneDraftValidator` under `src/` must return no production runtime type.

The plan artifact itself can be validated with:

```text
scripts/scherzo-execplan validate docs/plans/LIV-238-general-structured-output-validators.html
```

Expect the validator to report that the ExecPlan artifact is valid.

## Rollout, Recovery, and Idempotence

Roll out generic validators additively. First accept and test `validators:` while legacy `validator` still works through compatibility lowering. Then migrate review workflow files and fixtures. Only remove the legacy field after `docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md` migration requirements are satisfied and a separate removal decision is made.

If the chosen JSON Schema helper cannot satisfy the contract, stop after Milestone 2, record the discovery in this plan, and revise the JSON Schema implementation approach before touching retry, artifacts, or fingerprinting. Do not silently ship a partial validator that claims JSON Schema support but only checks a few fields.

If command validator diagnostics are too large, deadlock, leak secrets in tests, or inherit undeclared token variables, treat that as a blocking safety bug. Keep command validators disabled until truncation, cleanup, and redaction tests pass.

The parser, validator runner, artifact migration, and fingerprint changes should be idempotent under repeated test runs. Fixture commands used in tests must not mutate the workspace. Artifact tests should write to test-managed temporary run roots and clean up through existing test helpers if such helpers exist.

Rollback is straightforward before legacy syntax is removed: disable new workflow declarations and keep the compatibility lowering path. After legacy removal, rollback requires restoring the compatibility parser or reverting the migration commit. The implementation should keep commits small enough that parser, JSON Schema helper, command runner, runtime pipeline, artifact, fingerprint, and migration changes can be reverted independently if needed. If a rollback would violate `docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md`, update the spec or document the temporary non-conformance before using it beyond emergency containment.

## Artifacts and Notes

Current review-native validation path, summarized from source inspection:

```text
workflow YAML validator: review_lane_draft
  -> src/scherzo/workflow_dag.gleam parses ReviewLaneDraftValidator
  -> src/scherzo/structured_output.gleam maps it to review_lane_draft
  -> python3 scripts/scherzo-review validate-structured-output --validator review_lane_draft
  -> accepted payload is redacted and persisted as a structured-output artifact
```

Target generic validation path:

```text
workflow YAML validators:
  - type: json_schema
  - type: command
  -> src/scherzo/workflow_dag.gleam parses generic validator declarations
  -> src/scherzo/structured_output.gleam performs baseline admission
  -> src/scherzo/structured_output_json_schema.gleam runs JSON Schema validators in declaration order
  -> src/scherzo/structured_output_command_validator.gleam runs command validators in declaration order with redacted JSON on stdin
  -> accepted redacted payload is persisted as a structured-output artifact
```

Do not include local absolute paths in workflow YAML, plan examples, fingerprints, or tests. Use repository-relative schema paths and command paths. Runtime environment variables may contain local paths, but tests and diagnostics should compare refs, identifiers, or placeholders rather than machine-specific path prefixes.

## Interfaces and Dependencies

At the end of implementation, the workflow DAG model should expose generic structured-output validators. The exact syntax may follow project style, but it should be equivalent to:

```text
pub type StructuredOutputValidator {
  JsonSchemaValidator(
    name: String,
    path: String,
    draft: Option(String),
  )
  CommandValidator(
    name: String,
    argv: List(String),
    timeout_ms: Int,
    working_directory: ValidatorWorkingDirectory,
    env: List(#(String, String)),
  )
}

pub type ValidatorWorkingDirectory {
  ValidatorInWorkspace
  ValidatorInRepository
  ValidatorInRunRoot
}
```

The structured-output validation layer should expose an error type equivalent to:

```text
pub type StructuredOutputError {
  StructuredOutputMissing(message: String, retryable: Bool)
  StructuredOutputTruncated(message: String, retryable: Bool)
  StructuredOutputInvalidJson(message: String, retryable: Bool)
  StructuredOutputSchemaInvalid(message: String, retryable: Bool)
  StructuredOutputValidatorFailed(
    validator_name: String,
    validator_type: String,
    code: String,
    message: String,
    retryable: Bool,
    diagnostic_summary: String,
    stdout_truncated: Bool,
    stderr_truncated: Bool,
  )
  StructuredOutputToolSourceInvalid(code: String, message: String, retryable: Bool)
}
```

The implementation may choose a different constructor layout to fit Gleam style, but callers must be able to ask for `error_code`, `error_message`, and `error_retryable` without parsing strings.

The command validator runner should expose a function equivalent to:

```text
pub fn run_command_validator(
  declaration: workflow_dag.CommandValidator,
  payload_json: String,
  context: ValidatorContext,
  secrets: List(String),
) -> Result(ValidatorPass, ValidatorFailure)
```

The JSON Schema validator runner should expose a function equivalent to:

```text
pub fn run_json_schema_validator(
  declaration: workflow_dag.JsonSchemaValidator,
  value: json_value.JsonValue,
  context: ValidatorContext,
  secrets: List(String),
) -> Result(ValidatorPass, ValidatorFailure)
```

`ValidatorContext` should include config directory, repository root, workflow ID, run ID, step ID, attempt index, run root, workspace path, artifact name, format, source type, optional source tool name, validator name, validator type, and validator index. Use repository-relative paths in declarations and resolve them inside the runner. Do not persist local runtime paths in retained artifact JSON except where existing artifact storage already records repository-relative artifact paths.

Add dependency `pkgs.python3Packages.jsonschema` in `devenv.nix` for `scripts/scherzo-json-schema-validate`. The helper is internal to Scherzo. It is not the workflow-declared command validator mechanism and should not appear in workflow YAML.

## Open Questions and Clarifications Needed

None that block implementation.

Deferred behavior is explicit in `docs/specs/STRUCTURED_OUTPUT_VALIDATOR_SPEC.md` section 16 and remains out of scope for this plan: non-JSON formats, combining multiple Pi tool calls into one artifact, `require_single: false`, `reject_sibling_tool_calls: false`, more than one automatic structured-output retry, operating-system sandboxing for command validators, streaming validation, command executable content hashing in fingerprints, unredacted payload delivery to workflow-declared command validators, success-time retention of command validator stdout/stderr, and the exact timing for legacy `validator` removal.
