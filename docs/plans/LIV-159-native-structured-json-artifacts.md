# Add native structured JSON artifacts for workflow agent steps

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo workflows can already run agent steps and command steps, and later steps can use text fields from prior step artifacts in prompt templates. What operators cannot do today is declare that an ordinary `kind: agent` step must finish with machine-readable JSON, have Scherzo validate that JSON, retain it as a first-class artifact, and pass the artifact path and metadata to later steps. That gap pushes higher-level workflows toward nested scripts that run their own agent harnesses and artifact contracts.

After this plan is implemented, a workflow author can add a `structured_output` block to an agent step in a workflow YAML file. Scherzo will capture that step's final agent response, require it to be valid JSON, validate a small declared object schema, write a retained JSON artifact under Scherzo's existing artifact state directory, and expose the artifact path, reference, checksum, byte count, and validation status to downstream prompt templates. Existing workflows that do not declare `structured_output` will behave as they do now.

## Problem Framing and Constraints

The operator problem is not that Scherzo lacks another review-specific script. The problem is that a normal workflow step cannot currently promise a structured result to the workflow engine. Review lanes, analysis lanes, preflight lanes, and future automation therefore have to either parse free-form prose or delegate to an external runner that duplicates parts of Scherzo: prompt dispatch, final-response capture, artifact retention, failure reporting, and retries. That is brittle and makes workflow behavior harder to inspect and recover.

This plan is limited to core Scherzo support for structured JSON outputs from `kind: agent` steps. It does not redesign the staged code-review workflow, does not cut over any existing review commands, and does not depend on `scripts/scherzo-review --agent-backend external` as a final architecture. The implementation must keep structured output opt-in so that every current workflow YAML file remains compatible by default.

The repository is a Gleam project. Production code under `src/` must satisfy the repository lint policy: do not add production `let assert`, `panic`, or `todo`; prefer explicit `Result` handling; and run `direnv exec . gleam run -m glinter` and `direnv exec . gleam run -m scherzo_lint` before considering the work complete. Tests may use assertions, but asynchronous workflow tests should prefer deterministic fake dependencies and `test/test_async.gleam` helpers over sleeps.

## Strategy Overview

Add one opt-in structured-output contract to workflow DAG parsing, then process that contract at the boundary where an agent step has already returned a `WorkerSuccess` but before Scherzo records the step as successful. This is the smallest place to make the behavior native: the agent runner remains responsible for talking to pi and returning the final response, while the workflow runner becomes responsible for deciding whether that response satisfies the workflow's declared artifact contract.

The structured artifact write must go through Scherzo's existing workflow persistence seam, `workflow_checkpoint.Writer`, rather than opening an artifact store directly from `workflow_run.gleam`. The ledger-backed writer already owns the `artifact_store.Store` used for step artifacts, and tests can inject fake writers. Extending this writer keeps structured artifacts inside the same persistence boundary as step artifacts and avoids hidden side effects that recovery code cannot reason about.

The first version supports JSON object payloads with a minimal schema: `type: object` plus a list of required top-level keys. This is deliberately not a full JSON Schema implementation. It is sufficient for the current platform need, keeps the feature dependency-free, and still gives downstream steps a dependable machine-readable contract. The YAML syntax leaves room to extend validation later without changing the basic workflow shape.

Structured artifacts are stored in the same `.scherzo-state/artifacts` tree used for step artifacts. The file is JSON and contains metadata plus the parsed, redacted agent payload under a `payload` field. The step artifact stores structured-output outcome information and, when an artifact was written, metadata about that retained structured artifact. The first version does not add a separate ledger event for the structured artifact file; the durable completion record remains the existing step artifact plus `StepAttemptFinished` checkpoint. A structured artifact without a corresponding successful step artifact is therefore an orphan retained artifact, not evidence that the step completed.

## Alternatives Considered

The simplest alternative is to keep asking agent prompts to output JSON and let later command steps parse `steps.<step_id>.final_response`. That is insufficient because Scherzo cannot distinguish valid JSON from prose, cannot fail early on missing or truncated output, cannot retain a separate artifact with checksum metadata, and cannot give operators a clear artifact path.

Another alternative is to add a review-specific script runner that continues to own review JSON contracts. That solves the staged-review workflow locally, but it leaves Scherzo without a reusable core capability and creates nested agent execution inside scripts. This plan rejects that direction for the final architecture.

A larger alternative is to implement complete JSON Schema validation and arbitrary artifact types now. That is more than the current need requires. It would add design and dependency risk before Scherzo has one native structured-output path. This plan chooses a small JSON-only contract with required top-level fields, explicit failure modes, and stable extension points.

## Risks and Countermeasures

The main risk is silently accepting partial or malformed JSON. The countermeasure is that structured output is required to parse as a whole final response after trimming whitespace. If the final response is missing when required, blank when required, invalid JSON, truncated, not an object when the schema requires an object, or missing required keys, Scherzo records the step as failed with a specific failure code and does not write successful structured artifact metadata.

The highest safety risk is leaking secrets into a new retained JSON file. Existing agent result artifacts and step artifacts already receive a secret list and redact human-readable retained text. Structured output must use the same secret list before persistence. The validator must decode the JSON value, recursively redact every JSON string leaf with `scherzo/log.redact("structured_output", value, secrets)`, re-encode the redacted value, and pass only that redacted payload to the artifact writer. This redaction is idempotent if the final response was already redacted earlier. Tests must prove that a fake secret present in agent JSON is absent from the retained structured artifact and from template locals.

Another risk is breaking existing workflow YAML files or prompt templates. The countermeasure is opt-in parsing: `structured_output` is optional and is valid only on agent steps. Existing `AgentStep` behavior remains unchanged when the field is absent. Existing `StepArtifact` JSON decoding must treat the new structured-output outcome field as optional, so old retained artifacts remain readable and appear as `not_configured` in template locals.

The `required: false` option creates a subtle correctness risk. Missing or blank optional output must mean "valid absence, no artifact," not "write an empty JSON file" and not "fail anyway." The countermeasure is to model validation success with two cases: `StructuredOutputPresent(redacted_payload_json)` and `StructuredOutputAbsent`. Workflow execution writes a structured artifact only for the present case. For the absent case it records a successful step artifact with structured-output status `absent`, no path, no reference, and no checksum.

A fifth risk is polluting the artifact cleanup mechanism with arbitrary JSON files. The countermeasure is to write structured artifacts with a Scherzo wrapper containing `schema_version` and `artifact_type: structured_output`, and to update local artifact classification so these files are treated as retained workflow artifacts. Corrupt or unreferenced structured-output wrappers must be retained for operator inspection, matching the existing conservative cleanup behavior for corrupt or unknown artifacts.

A sixth risk is exposing an artifact path that downstream steps cannot use. The countermeasure is to expose both a stable artifact-store reference and the runtime filesystem path for successful structured outputs. Tests should assert the reference shape and that the path points to a readable file, without hard-coding an absolute local prefix.

A seventh risk is two-file inconsistency. A successful structured-output step writes a structured artifact file and then a step artifact that points at it. The ordering must be explicit: write the structured artifact atomically first; if that write fails, return a failed step artifact with `failure_code: structured_output_artifact_write_failed` and no structured metadata; if the later step artifact write or `step_finished` checkpoint fails, existing checkpoint failure handling applies and recovery must not treat the orphan structured artifact as proof that the step completed. Local artifact classification must retain such orphan files.

The final risk is making recovery and retries special. The countermeasure is to model structured-output validation failure and structured-artifact write failure as normal failed step artifacts. Existing failure policy, workflow summaries, retry, recovery, and checkpoint paths continue to operate on `StepArtifact` and `WorkflowRunFailure` rather than introducing a second failure channel.

## Progress

- [x] (2026-05-08 00:00Z) Drafted this ExecPlan for LIV-159 from the Linear issue and current repository orientation.
- [x] (2026-05-08 00:00Z) Incorporated adversarial review findings covering redaction, checkpoint writer integration, optional-output semantics, write-failure recovery, parser defaults, and step granularity.
- [ ] Add workflow DAG parser tests for explicit structured-output contracts, parser defaults, invalid fields, and command-step rejection.
- [ ] Implement workflow DAG structured-output types and parsing while preserving existing agent-step behavior when absent.
- [ ] Add pure validator tests for required output, optional absent output, invalid nonblank optional output, truncation, schema failures, and secret redaction.
- [ ] Implement pure JSON validation, optional absence, recursive string redaction, and stable structured-output error codes.
- [ ] Extend `workflow_checkpoint.Writer` and `artifact_store` with atomic structured-output artifact writing through the existing persistence seam.
- [ ] Add artifact-store and local-artifact tests for retained, corrupt, and unreferenced structured-output files.
- [ ] Extend `StepArtifact` encoding, decoding, helper constructors, and template locals for valid, absent, error, and not-configured structured-output outcomes.
- [ ] Integrate structured-output validation and writer calls into workflow agent-step execution, including write-failure behavior.
- [ ] Add workflow-run tests for valid JSON, missing required output, optional missing and blank output, optional invalid output, invalid JSON, truncation, schema invalidity, redacted retained payloads, artifact write failure, downstream template metadata, and unchanged legacy workflows.
- [ ] Update workflow fingerprint behavior if the compiler or tests show structured-output specs are not represented.
- [ ] Run full validation and update Outcomes & Retrospective.

## Surprises & Discoveries

- Observation: `src/scherzo/step_artifact.gleam` already captures agent `final_response`, failure metadata, truncation flags, workflow summaries, and template locals with dotted keys such as `steps.code_review.final_response`.
  Evidence: the module defines `StepArtifact`, `from_agent_success`, `to_json`, `decoder`, `to_template_locals`, and `workflow_result_artifact`.
- Observation: `src/scherzo/workflow_dag.gleam` is the workflow YAML parser and currently models agent steps as `AgentStep(prompt: PromptRef)` and command steps as `CommandStep(run: String, timeout_ms: Option(Int))`.
  Evidence: the parser reads `kind: agent` plus `prompt` and has tests in `test/workflow_dag_test.gleam`.
- Observation: `src/scherzo/workflow_run.gleam` already uses fakeable dependencies for `agent_step` and `command_step`, which is the right seam for deterministic structured-output tests without live model calls.
  Evidence: `workflow_run.Dependencies` includes injected `agent_step`, `command_step`, workspace preparation, cleanup, and a `checkpoint` writer; `test/workflow_run_test.gleam` uses these fakes.
- Observation: retained step artifacts already live under `.scherzo-state/artifacts` through `src/scherzo/state/artifact_store.gleam`.
  Evidence: `artifact_store.write_step_artifact` writes schema-versioned JSON files using refs like `runs/<run>/<step>/attempt-<n>.json`.
- Observation: workflow checkpoint persistence is centralized in `src/scherzo/workflow_checkpoint.gleam`, where `Writer.write_step_artifact` delegates to `artifact_store.write_step_artifact` for the ledger writer and can be faked by `noop_writer` or tests.
  Evidence: `workflow_run.gleam` calls `dependencies.checkpoint.write_step_artifact` before `dependencies.checkpoint.step_finished`, and `workflow_checkpoint.ledger_writer` constructs the artifact store.
- Observation: agent result final responses may already be redacted and truncated before becoming a `WorkerSuccess`, but structured-output persistence still needs its own explicit redaction boundary.
  Evidence: `src/scherzo/result_artifact.gleam` calls `log.redact("assistant_output", text, secrets)` before storing `final_response`, and `src/scherzo/step_artifact.gleam` redacts/caps again in `from_agent_success`.

## Decision Log

- Decision: Make structured output opt-in with a `structured_output` block on `kind: agent` steps only.
  Rationale: This preserves compatibility for all current workflows and keeps command-step behavior unchanged.
  Date: 2026-05-08
- Decision: Use `format: json` with a minimal object schema, not full JSON Schema.
  Rationale: Required top-level keys are enough to make first native artifacts machine-readable and testable while avoiding a broad schema dependency or validator implementation.
  Date: 2026-05-08
- Decision: Require the trimmed final response to be JSON only; do not accept prose around the JSON in the first version.
  Rationale: Whole-response parsing is deterministic, easy to explain in prompts, and prevents ambiguous extraction. A future version can add explicit extraction modes if operators need fenced JSON blocks or mixed prose.
  Date: 2026-05-08
- Decision: Write retained structured artifacts as Scherzo-wrapped JSON files with a `payload` field.
  Rationale: The wrapper gives cleanup, recovery, diagnostics, and downstream commands stable metadata while keeping the agent's JSON payload machine-readable.
  Date: 2026-05-08
- Decision: Treat structured-output validation failures and structured-artifact write failures as normal failed step artifacts.
  Rationale: Existing failure policy, summaries, retries, recovery, and checkpointing are already centered on `StepArtifact`; using the same channel reduces special cases.
  Date: 2026-05-08
- Decision: Extend `workflow_checkpoint.Writer` with a structured-artifact write function and call that function from workflow execution instead of constructing an artifact store in `workflow_run.gleam`.
  Rationale: The checkpoint writer is the existing persistence boundary for workflow artifacts. Reusing it keeps production, recovery, and fake-test behavior aligned and makes structured artifact writing injectable for failure tests.
  Date: 2026-05-08
- Decision: Do not add a separate ledger event for the structured artifact file in the first version.
  Rationale: The step artifact metadata and existing `StepAttemptFinished` record remain the durable completion signal. A structured artifact file without a matching successful step artifact is an orphan retained artifact and must not be interpreted as completed work.
  Date: 2026-05-08
- Decision: Validate JSON first, then recursively redact every JSON string value before writing the retained structured artifact.
  Rationale: Parsing first proves the output is structurally valid, and recursive value redaction ensures a valid JSON artifact cannot bypass the existing secret-redaction policy. Applying redaction again is safe when upstream result text was already redacted.
  Date: 2026-05-08
- Decision: Model validation success as either `StructuredOutputPresent` or `StructuredOutputAbsent`.
  Rationale: `required: false` needs a first-class success case for missing or blank output that writes no artifact, keeps the step successful, and remains distinguishable from both configured failures and unconfigured legacy steps.
  Date: 2026-05-08
- Decision: Represent structured-output state in step artifacts as valid, absent, error, or not configured.
  Rationale: Downstream templates and operators need to distinguish a successful optional absence from a workflow that never requested structured output. Old artifacts decode as not configured.
  Date: 2026-05-08
- Decision: Keep production workflow YAML adoption out of this implementation plan.
  Rationale: The core feature is additive and opt-in, but committing a production workflow that uses the new syntax would make rollback to an older build require a workflow-file edit. Test fixtures may use the syntax; production workflow cutover belongs in a later rollout plan.
  Date: 2026-05-08
- Decision: Incorporate every blocking and gap finding from the adversarial review; reject none.
  Rationale: The review findings identified real implementation and safety gaps around redaction, persistence, optional output, tests, and step size. The revised plan closes those gaps rather than deferring them to implementation.
  Date: 2026-05-08

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo workflows are YAML documents parsed by `src/scherzo/workflow_dag.gleam`. A workflow has `steps`, and each step is currently either an agent step with a prompt or a command step with a shell command. The parser returns a `workflow_dag.WorkflowDag` containing `workflow_dag.WorkflowStep` values. The workflow runner in `src/scherzo/workflow_run.gleam` prepares workspaces, renders prompts with template locals, executes steps, converts outputs into `step_artifact.StepArtifact` values, records checkpoints, and returns a workflow-level result.

A step artifact is Scherzo's retained summary of one step attempt. `src/scherzo/step_artifact.gleam` defines `StepArtifact`, encodes and decodes it as JSON, creates artifacts from successful agent results and command results, exposes fields to templates, and builds the workflow result summary. Current template locals are flat dotted keys represented as strings, integers, booleans, lists, or nil values by `src/scherzo/template.gleam`.

The workflow checkpoint writer in `src/scherzo/workflow_checkpoint.gleam` is the persistence boundary used by workflow execution. `workflow_run.Dependencies` contains a `checkpoint: workflow_checkpoint.Writer`, and `workflow_run.gleam` calls that writer to persist step artifacts and append step-finished records. The ledger writer constructs the artifact store internally. Structured-output artifact writing must extend this writer so workflow execution continues to use injected persistence instead of directly constructing an artifact store.

The artifact store in `src/scherzo/state/artifact_store.gleam` writes retained step artifacts below `.scherzo-state/artifacts` under a run-specific reference. The local artifact scanner in `src/scherzo/state/local_artifacts.gleam` classifies retained artifacts for recovery and cleanup. Structured output must use these state mechanisms rather than inventing an unrelated directory.

Agent execution returns `agent_types.WorkerSuccess` values containing `result_artifact.ResultArtifact`. That result has `final_response: Option(String)`, `truncated: Bool`, and `source: String`. `StepArtifact.from_agent_success` currently copies this final response into the step artifact after capping and redaction. Structured-output parsing should use `success.result.final_response` from the `WorkerSuccess`, not `StepArtifact.final_response`, because the step artifact may cap text for templates. Structured-output parsing must fail if `success.result.truncated` is true or if the later step artifact marks the response truncated.

Secret redaction is part of the persistence boundary. The workflow runner already receives `secrets: List(String)` and passes them to step-artifact construction. Structured-output validation must also receive this list and redact the structured payload before any retained structured artifact is written.

## Preconditions and Verified Facts

The repository root contains `src/scherzo/workflow_dag.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/workflow_checkpoint.gleam`, `src/scherzo/step_artifact.gleam`, `src/scherzo/template.gleam`, `src/scherzo/result_artifact.gleam`, `src/scherzo/state/artifact_store.gleam`, and `src/scherzo/state/local_artifacts.gleam`.

The current workflow DAG syntax uses `version: 1`, top-level `id`, optional top-level `workspace_profile`, optional `max_parallel_steps`, and a `steps` list. Agent steps use `kind: agent` and `prompt`; command steps use `kind: command` and `run`. The parser validates step IDs and workspace names and already rejects unknown step kinds and malformed fields.

The current `workflow_checkpoint.Writer` contains callbacks for workflow finished, step prepared, step started, step continuation started, pi-session observation, step artifact writing, step finished, and step interrupted. `workflow_checkpoint.ledger_writer` owns the `artifact_store.Store` and maps artifact-store failures into `CheckpointArtifactFailed`. This plan depends on extending that writer rather than bypassing it.

The current `StepArtifact` fields include step ID, status, final response, command exit metadata, diagnostic path, failure code, stdout, stderr, truncation booleans, and summary text. Its decoder uses optional fields for nullable values and should be extended with optional structured-output fields so older artifacts decode successfully.

The current template system supports locals passed into `template.render_with_locals` and `template.render_scheduled_with_locals`. `step_artifact.to_template_locals` is the existing central function that turns prior step artifacts into downstream template variables.

The current workflow tests in `test/workflow_run_test.gleam` already fake agent execution through `workflow_run.Dependencies`, so new tests must follow that pattern and must not call a live model or pi process. To test structured-artifact write failures, use an injected `workflow_checkpoint.Writer` whose new structured writer callback returns `CheckpointArtifactFailed("structured write failed")`.

## Scope Boundaries

In scope: parsing an opt-in structured-output declaration on agent steps; validating JSON final responses for configured steps; writing a retained structured artifact; attaching structured artifact metadata to `StepArtifact`; exposing metadata to downstream templates; preserving workflow summaries, diagnostics, retries, recovery, and cleanup behavior; and adding deterministic tests.

Out of scope: changing the staged code-review workflow, changing `scripts/scherzo-review`, introducing a new agent backend, adding non-JSON artifact formats, implementing complete JSON Schema, adding UI for browsing structured artifacts, or changing the Linear workflow labels and dispatch policy.

Existing workflows without `structured_output` must continue to parse and execute unchanged. Existing command steps must not accept `structured_output`; if a command step declares it, workflow DAG parsing must fail clearly rather than silently ignoring it.

## YAML Interface

Add this syntax to workflow YAML files:

    version: 1
    id: structured_review
    steps:
      - id: review_json
        kind: agent
        prompt: prompts/review.md
        structured_output:
          format: json
          artifact_name: review_result
          required: true
          schema:
            type: object
            required:
              - summary
              - findings

The fields have these semantics:

`format` is optional and defaults to `json`. The first implementation accepts only `json`; any other value fails DAG parsing with `unsupported_structured_output_format`.

`artifact_name` is optional and defaults to the step ID. It is used as the final path component of the structured artifact reference and must be a safe lowercase identifier using the same style as workflow step IDs: lowercase letters, numbers, and underscores. Invalid names fail DAG parsing with `invalid_structured_artifact_name`.

`required` is optional and defaults to `true`. When `true`, a missing, blank, truncated, invalid, or schema-invalid final response fails the step. When `false`, a missing or blank final response with no truncation produces no structured artifact, records the structured-output status as `absent`, and leaves the step successful. When `false` and a nonblank response is present, the response is still parsed, schema-validated, redacted, and written as a structured artifact; invalid nonblank JSON still fails because the workflow declared that any provided structured output must be valid. If the agent result is marked truncated, the step fails with `structured_output_truncated` even when `required: false`, because Scherzo cannot safely tell whether a missing or blank-looking response was complete.

`schema.type` is optional and defaults to `object`. The first implementation accepts only `object`; other values fail DAG parsing with `unsupported_structured_output_schema_type`.

`schema.required` is optional and defaults to an empty list. Each item must be a safe top-level JSON object key string. During execution, all listed keys must be present in the parsed JSON object. Missing keys fail the step with `structured_output_schema_invalid`.

The final agent response for a configured step must be JSON only after trimming leading and trailing whitespace. Do not accept explanatory prose before or after the JSON in this first version. Prompt authors should instruct the agent to end with only the JSON object.

## Structured Artifact Format

Structured-output files are written through `workflow_checkpoint.Writer.write_structured_output_artifact`, whose ledger implementation delegates to `artifact_store.write_structured_output_artifact`. Do not instantiate `artifact_store.Store` directly in `src/scherzo/workflow_run.gleam`.

Write retained structured artifacts under the existing artifact root using this reference shape:

    runs/<safe-run-id>/<step-id>/attempt-<attempt-index>/structured/<artifact-name>.json

The runtime filesystem path is the artifact root plus that reference. Template locals should expose both the reference and the path for successful structured outputs. Tests must not hard-code an absolute local path; they should assert the reference exactly and assert that the path ends with `.scherzo-state/artifacts/<ref>` or that reading the returned path succeeds.

The file content is JSON with this shape:

    {
      "schema_version": 1,
      "artifact_type": "structured_output",
      "run_id": "run-1",
      "workflow_id": "structured_review",
      "step_id": "review_json",
      "attempt_index": 0,
      "artifact_name": "review_result",
      "format": "json",
      "schema": {
        "type": "object",
        "required": ["summary", "findings"]
      },
      "payload": {
        "summary": "ready",
        "findings": []
      }
    }

The `payload` value is the parsed JSON object from the agent's final response after recursive secret redaction of all string values. The artifact-store writer receives only this redacted payload value or a redacted canonical JSON string; it must never receive the unredacted final response. The wrapper makes the artifact self-describing for cleanup and recovery while preserving the machine-readable payload.

After writing the file atomically, the writer reads or hashes the written bytes and returns a structured-artifact metadata value containing the artifact reference, runtime path, SHA-256 checksum, and byte count. The checksum and byte count describe the full wrapped JSON file, not only the payload.

## Failure Semantics

For a configured required structured output, Scherzo must fail the step clearly in these cases:

A missing final response, a `None` final response, or a response that is blank after trimming fails with failure code `structured_output_missing` and an error message that names the step ID and says that a JSON final response was required.

For a configured optional structured output with `required: false`, a missing final response or blank response succeeds only when the agent result is not marked truncated. The step artifact records structured-output status `absent`, records the configured artifact name and format for diagnostics, writes no structured artifact file, and exposes no path, reference, checksum, or byte count.

A truncated response fails with failure code `structured_output_truncated` before parsing. Treat either the agent result's `truncated` flag or the step artifact's final-response truncation flag as unsafe. Do not write a structured artifact from truncated text, even when `required: false`.

A nonblank response that is not valid JSON as a whole trimmed string fails with failure code `structured_output_invalid_json` and an error message that says the final response must contain JSON only.

A valid JSON response whose top-level value is not an object when `schema.type: object` is configured fails with failure code `structured_output_schema_invalid`.

A valid JSON object that lacks one or more required top-level keys fails with failure code `structured_output_schema_invalid`; include the missing keys in the diagnostic text.

A valid present structured output whose retained artifact cannot be written fails with failure code `structured_output_artifact_write_failed`. The failed step artifact must include a concise message derived from `workflow_checkpoint.describe_error`, must not include successful structured-output metadata, and must not claim a path or checksum for an artifact whose write failed.

Validation failures and write failures create a `StepArtifact` with `status: StepFailed`, `failure_code` set to the code above, the capped/redacted final response retained for debugging when available, and structured-output outcome status `error` with a concise human-readable reason. The step then flows through the existing `on_failure` policy. With the default `on_failure: fail`, the workflow fails. With `on_failure: continue`, downstream steps may still run and can inspect the failed step artifact.

If the structured artifact write succeeds but the later step-artifact write or `step_finished` checkpoint fails, the workflow returns the existing checkpoint failure. Recovery must rely on the step artifact and step-finished checkpoint, not on the structured file alone. The structured file may remain under `.scherzo-state/artifacts` as an orphan retained artifact, and local artifact cleanup must retain it conservatively.

## Milestones

Milestone 1 adds the data model and YAML parsing. At the end, `workflow_dag.parse` accepts valid `structured_output` blocks on agent steps, applies defaults for omitted fields, rejects invalid blocks with stable error codes, rejects `structured_output` on command steps, and all existing workflow DAG tests still pass after updating constructors and patterns for the new agent-step shape.

Milestone 2 adds pure structured-output validation. At the end, a new module can turn final-response text plus a schema, truncation flag, and secret list into either `StructuredOutputPresent(redacted_payload_json)`, `StructuredOutputAbsent`, or a specific error. This milestone proves optional absence and redaction before any file-writing code exists.

Milestone 3 adds checkpoint-writer and artifact-store support. At the end, `workflow_checkpoint.Writer` has an injectable structured-artifact write callback, the ledger writer stores a schema-versioned structured-output artifact under `.scherzo-state/artifacts`, the noop writer remains usable in tests, and artifact-store tests can verify checksum, byte count, wrapper shape, redacted payload, and reference shape.

Milestone 4 extends step artifacts and template locals. At the end, `StepArtifact` can encode, decode, and expose structured-output outcomes for valid, absent, error, and not-configured states. Old retained step artifacts still decode and appear as `not_configured`.

Milestone 5 integrates structured output into workflow agent execution. At the end, an agent step with a valid JSON final response writes a retained structured artifact through the checkpoint writer and records metadata in its `StepArtifact`; optional absent output succeeds without an artifact; invalid, missing, truncated, schema-invalid, or write-failed output produces a failed step artifact and a clear workflow failure.

Milestone 6 validates compatibility, recovery, cleanup, fingerprinting, and linting. At the end, existing workflows without `structured_output` have unchanged behavior, local artifact cleanup recognizes valid and orphan structured artifacts, workflow summaries remain stable except for configured structured steps, workflow fingerprints account for structured-output specs if needed, and the full test and lint commands pass.

## Plan of Work

In `src/scherzo/workflow_dag.gleam`, add public types for the structured-output declaration. Use names close to these so downstream code remains readable:

    pub type StructuredOutputFormat {
      StructuredJson
    }

    pub type StructuredOutputSchema {
      StructuredObjectSchema(required_keys: List(String))
    }

    pub type StructuredOutputSpec {
      StructuredOutputSpec(
        artifact_name: String,
        required: Bool,
        format: StructuredOutputFormat,
        schema: StructuredOutputSchema,
      )
    }

Change the agent step variant from `AgentStep(prompt: PromptRef)` to `AgentStep(prompt: PromptRef, structured_output: Option(StructuredOutputSpec))`. Keep command steps unchanged. Update helper functions such as `prompt_file_path` and `with_prompt` to preserve the structured-output option when changing a prompt. Update every production pattern match that currently expects `AgentStep(prompt)`.

Still in `src/scherzo/workflow_dag.gleam`, parse a `structured_output` map only when the step kind is agent. Use existing private helper style for required and optional YAML fields. Default `format` to `json`, `artifact_name` to the current step ID, `required` to `true`, `schema.type` to `object`, and `schema.required` to an empty list. Validate `artifact_name` and required keys. Reject `structured_output` on command steps with `structured_output_on_command_step`.

Create `src/scherzo/structured_output.gleam`. This module should be pure except for receiving already captured strings and a secret list. Define a validation result type and error type like this:

    pub type StructuredOutputValidation {
      StructuredOutputPresent(payload_json: String)
      StructuredOutputAbsent
    }

    pub type StructuredOutputError {
      StructuredOutputMissing(message: String)
      StructuredOutputTruncated(message: String)
      StructuredOutputInvalidJson(message: String)
      StructuredOutputSchemaInvalid(message: String)
    }

    pub fn validate_final_response(
      spec: workflow_dag.StructuredOutputSpec,
      final_response: Option(String),
      truncated: Bool,
      secrets: List(String),
    ) -> Result(StructuredOutputValidation, StructuredOutputError)

The validator should trim whitespace, enforce the required/optional/truncated rules, parse the whole string as JSON, validate the object schema, recursively redact every JSON string value with `scherzo/log.redact("structured_output", value, secrets)`, and return a redacted canonical JSON payload string. The function should expose helpers for a stable error code and human message so the workflow runner never pattern-matches on message text. If the Gleam JSON library does not expose a convenient generic JSON value decoder, implement a small internal JSON-value representation in this module and decode into it with `gleam/dynamic/decode`; do not add a dependency solely for this first schema.

In `src/scherzo/workflow_checkpoint.gleam`, extend the persistence interface before changing workflow execution. Add request and response types equivalent to:

    pub type StructuredOutputWrite {
      StructuredOutputWrite(
        run_id: String,
        workflow_id: String,
        step_id: String,
        attempt_index: Int,
        artifact_name: String,
        format: String,
        schema_required_keys: List(String),
        payload_json: String,
      )
    }

    pub type StructuredArtifactWritten {
      StructuredArtifactWritten(
        ref: String,
        path: String,
        sha256: String,
        bytes: Int,
      )
    }

Add `write_structured_output_artifact: fn(StructuredOutputWrite) -> Result(StructuredArtifactWritten, CheckpointError)` to `Writer`. `noop_writer` should return a deterministic `noop/<step>/attempt-<n>/structured/<artifact-name>.json` reference, a nonempty placeholder path, `sha256: "noop"`, and `bytes: 0`. `ledger_writer` should delegate to the artifact store and map errors to `CheckpointArtifactFailed`. Do not append a separate ledger record for this file in the first version.

In `src/scherzo/state/artifact_store.gleam`, add a structured-output artifact writer. Reuse the existing atomic write, path validation, hashing, and safe component helpers rather than duplicating file I/O. The writer should accept the fields from `workflow_checkpoint.StructuredOutputWrite`, parse or embed the already-redacted payload JSON under the wrapper's `payload` field, write the wrapped JSON file, compute SHA-256 and byte size, and return metadata containing the artifact reference, runtime path, SHA-256, and byte count. Add a read helper for tests that verifies checksum and decodes the wrapper.

In `src/scherzo/state/local_artifacts.gleam`, extend artifact classification so `artifact_type: structured_output` files with the new schema are considered current workflow artifacts. Corrupt structured-output wrappers and unreferenced structured-output wrappers should be retained for operator inspection, matching the existing conservative cleanup behavior for corrupt or unknown artifacts.

In `src/scherzo/step_artifact.gleam`, add public metadata and outcome types for structured output. Use an outcome shape that can represent all four downstream states:

    pub type StructuredOutputMetadata {
      StructuredOutputMetadata(
        artifact_name: String,
        format: String,
        ref: String,
        path: String,
        sha256: String,
        bytes: Int,
        schema_status: String,
      )
    }

    pub type StructuredOutputOutcome {
      StructuredOutputValid(StructuredOutputMetadata)
      StructuredOutputAbsent(artifact_name: String, format: String, schema_status: String)
      StructuredOutputError(artifact_name: String, format: String, message: String)
    }

Extend `StepArtifact` with `structured_output: Option(StructuredOutputOutcome)`. Encode this as an optional nested JSON object and decode it as optional so old retained artifacts remain readable. Add helpers for constructing a successful agent artifact with valid structured-output metadata, a successful agent artifact with absent optional structured output, and a failed structured-output artifact. Do not remove or repurpose the existing `final_response` field; it remains the capped text useful for human diagnostics and summaries.

Update `step_artifact.to_template_locals` to add these dotted locals for every step artifact:

    steps.<step_id>.structured_output.status
    steps.<step_id>.structured_output.artifact_name
    steps.<step_id>.structured_output.format
    steps.<step_id>.structured_output.ref
    steps.<step_id>.structured_output.path
    steps.<step_id>.structured_output.sha256
    steps.<step_id>.structured_output.bytes
    steps.<step_id>.structured_output.schema_status
    steps.<step_id>.structured_output.error

Use `valid` for a successful structured artifact, `absent` for a configured optional output that produced no artifact, `error` for a structured-output validation or write failure, and `not_configured` when the step has no structured-output outcome at all. Use `template.VNil` for unavailable scalar fields so templates can render them as empty strings if referenced.

In `src/scherzo/workflow_run.gleam`, integrate validation at the production call site that turns an agent `WorkerSuccess` into a step artifact. Update the `AgentStep` pattern to `AgentStep(prompt_ref, structured_output)`. For `AgentStep(_, None)`, preserve the current behavior exactly by calling `step_artifact.from_agent_success` as today. For `AgentStep(_, Some(spec))`, call `structured_output.validate_final_response(spec, success.result.final_response, success.result.truncated, secrets)` before marking the step successful.

If validation returns `StructuredOutputAbsent`, create a successful agent artifact with structured-output outcome `absent` and do not call the structured-artifact writer. If validation returns `StructuredOutputPresent(payload_json)`, call `dependencies.checkpoint.write_structured_output_artifact` with the run ID, workflow ID, step ID, attempt index, artifact name, schema summary, and redacted payload. If the writer succeeds, attach the returned metadata to the successful `StepArtifact`. If the writer fails, create a failed `StepArtifact` with `failure_code: structured_output_artifact_write_failed`, structured-output outcome `error`, and no successful metadata.

If validation returns an error, create a failed `StepArtifact`, include the stable structured-output failure code and message, record the structured-output outcome as `error`, and return a `StepExecutionResult` whose artifact is failed. Keep workflow-level failure handling unchanged: the scheduler should see the failed artifact and apply `on_failure`. The workflow failure reason should include the structured-output failure code and message so operators do not have to open the artifact JSON to understand the failure.

Update `src/scherzo/workflow_fingerprint.gleam` if it serializes or hashes `WorkflowStep` values. The workflow fingerprint must change when a step adds, removes, or changes `structured_output`, because that changes workflow behavior and retained artifacts.

Do not add or modify production workflow YAML files to use `structured_output` in this plan. Test fixtures may include the new syntax. Let the compiler identify remaining pattern matches for `AgentStep`. Update them mechanically, preserving old behavior when the structured-output option is `None`. Do not add production `let assert`, `panic`, or `todo` while making these updates.

## Concrete Steps

1. From the repository root, inspect the clean tree with:

       jj status --color=never

   Expect a clean working copy before implementation begins.

2. In `test/workflow_dag_test.gleam`, add `parses_agent_structured_output_defaults_test`. Parse an agent step with an empty `structured_output` map or with only `structured_output: {}` in the existing YAML style. Assert the parsed spec defaults to `format == StructuredJson`, `artifact_name == "review_json"`, `required == True`, `schema == StructuredObjectSchema([])`, and the prompt remains `PromptFile("prompts/review.md")`.

3. In `test/workflow_dag_test.gleam`, add `parses_agent_structured_output_json_contract_test` using the YAML shown in the YAML Interface section. Assert the step kind is `AgentStep(PromptFile("prompts/review.md"), Some(spec))`, with `artifact_name == "review_result"`, `required == True`, `format == StructuredJson`, and required keys `summary` and `findings`.

4. In `test/workflow_dag_test.gleam`, add parser rejection tests for `structured_output` on command steps, unsupported format, invalid artifact name, unsupported schema type, non-list `schema.required`, and a non-string required key. Each test should assert the stable error code named in this plan.

5. Run a targeted parser test command:

       direnv exec . gleam test --target erlang test/workflow_dag_test.gleam

   If the project test runner does not accept a file argument in the current Gleam version, run `direnv exec . gleam test` instead. Expect compile errors or failing new parser tests before the implementation exists.

6. Edit `src/scherzo/workflow_dag.gleam` to add the structured-output types and change the agent-step variant to `AgentStep(prompt: PromptRef, structured_output: Option(StructuredOutputSpec))`.

7. Still in `src/scherzo/workflow_dag.gleam`, implement parsing and validation for `structured_output`, including all defaults and stable parse error codes.

8. Update existing `AgentStep` pattern assertions in `test/workflow_dag_test.gleam` to include `None` where a workflow has no structured-output block.

9. Run the parser tests again. After Milestone 1, expect the workflow DAG tests to pass. If the compiler reports production `AgentStep` pattern matches, update those matches mechanically to preserve old behavior for `None` and rerun until this milestone is green.

10. Create `test/structured_output_test.gleam`. Add validator tests for these required-output inputs: `Some("{\"summary\":\"ok\",\"findings\":[]}")` with required keys `summary` and `findings` returns `StructuredOutputPresent`; `Some("not json")` returns `structured_output_invalid_json`; `None` returns `structured_output_missing`; `Some("   ")` returns `structured_output_missing`; a truncated flag returns `structured_output_truncated`; `Some("[]")` returns `structured_output_schema_invalid`; and `Some("{\"summary\":\"ok\"}")` returns `structured_output_schema_invalid` naming `findings`.

11. In `test/structured_output_test.gleam`, add optional-output tests. With `required: false`, `None` returns `StructuredOutputAbsent`, `Some("   ")` returns `StructuredOutputAbsent`, valid JSON returns `StructuredOutputPresent`, and nonblank invalid JSON returns `structured_output_invalid_json`. Add a truncated optional case that returns `structured_output_truncated`.

12. In `test/structured_output_test.gleam`, add `redacts_secret_strings_before_returning_payload_test`. Use final response `{"summary":"token-123","findings":["token-123"]}` and a secrets list containing `token-123`. Assert the returned present payload does not contain `token-123`, still parses as JSON, and still contains keys `summary` and `findings`.

13. Run:

       direnv exec . gleam test

   Expect the new structured-output validator tests to fail or fail to compile before the module exists.

14. Create `src/scherzo/structured_output.gleam` with `StructuredOutputValidation`, `StructuredOutputError`, stable error-code helpers, human-message helpers, whole-response JSON parsing, schema validation, optional absence, truncation checks, and recursive string redaction.

15. Run `direnv exec . gleam test` again. After Milestone 2, expect `test/structured_output_test.gleam` and the parser tests to pass.

16. Edit `src/scherzo/workflow_checkpoint.gleam` to add `StructuredOutputWrite`, `StructuredArtifactWritten`, and a `write_structured_output_artifact` callback to `Writer`. Update `noop_writer` to return deterministic placeholder metadata and update `ledger_writer` to call a not-yet-created artifact-store function.

17. Run `direnv exec . gleam test`. Expect compiler errors that identify the missing artifact-store function or any tests that construct `workflow_checkpoint.Writer` directly.

18. Add `test/structured_artifact_store_test.gleam`. Test that writing redacted payload `{"summary":"ok","findings":[]}` returns ref `runs/run-1/review_json/attempt-0/structured/review_result.json`, returns a readable path, returns a nonempty SHA-256 and positive byte count, writes `artifact_type: structured_output`, and stores the payload under `payload`.

19. In `test/structured_artifact_store_test.gleam`, add a redaction-boundary assertion using a payload string that already contains only the redacted value from the validator. Assert the artifact file does not contain the raw fake secret used in the validator test.

20. Edit `src/scherzo/state/artifact_store.gleam` to add the structured-output reference builder, wrapper encoder/decoder, writer, and test read helper. Reuse the existing atomic write path and safe path-component helpers.

21. Run `direnv exec . gleam test`. After Milestone 3 artifact-store work, expect the structured artifact-store tests to pass and no writer-interface compile errors to remain.

22. Add or extend local-artifact tests near the existing local artifact cleanup tests. The exact file name may already exist; if it does, append there. If not, create `test/local_artifacts_structured_output_test.gleam`. Add one test for a valid structured-output wrapper and one test for an unreferenced or corrupt structured-output wrapper. Assert both are retained or classified conservatively rather than deleted as junk.

23. Edit `src/scherzo/state/local_artifacts.gleam` so structured-output wrappers are classified as current workflow artifacts and corrupt or unreferenced wrappers are retained for operator inspection.

24. Run `direnv exec . gleam test` and expect the local-artifact tests to pass.

25. In `test/step_artifact_test.gleam`, add `structured_output_metadata_encodes_decodes_and_exposes_template_locals_test`. Build a `StepArtifact` with `StructuredOutputValid` metadata and assert JSON round-trip preserves the metadata. Assert `steps.review_json.structured_output.status == template.VString("valid")`, path and ref are exposed, checksum is exposed, and bytes is exposed as `template.VInt`.

26. In `test/step_artifact_test.gleam`, add `optional_absent_structured_output_exposes_absent_status_test`. Build a successful artifact with `StructuredOutputAbsent("review_result", "json", "not_applicable")` and assert status is `absent`, artifact name and format are present, path/ref/checksum/bytes are `template.VNil`, and error is `template.VNil`.

27. In `test/step_artifact_test.gleam`, add `structured_output_error_exposes_error_status_test`. Build a failed artifact with structured-output outcome `error`, assert the stable failure code is encoded, and assert the error template local contains the human-readable reason.

28. In `test/step_artifact_test.gleam`, add `step_without_structured_output_exposes_not_configured_status_test`. Decode or build an existing-style artifact with no structured-output field and assert status is `not_configured` and metadata locals are nil.

29. Edit `src/scherzo/step_artifact.gleam` to add `StructuredOutputMetadata`, `StructuredOutputOutcome`, JSON encoding, JSON decoding with optional defaults, template locals, and helper constructors. Keep this edit focused on step-artifact state; do not change workflow execution in the same edit.

30. Run `direnv exec . gleam test`. After Milestone 4, expect all step-artifact tests to pass.

31. In `test/workflow_run_test.gleam`, add `valid_json_final_response_becomes_retained_structured_artifact_test`: construct a workflow DAG with an agent step declaring structured output; have the fake agent return final response `{"summary":"ok","findings":[]}`; execute the workflow; assert the step artifact succeeded; assert structured-output metadata is present; read the metadata path; assert the retained file has `artifact_type: structured_output` and the expected payload.

32. In `test/workflow_run_test.gleam`, add required failure tests: `invalid_json_structured_output_fails_agent_step_clearly_test`, `missing_required_structured_output_fails_agent_step_clearly_test`, `truncated_structured_output_fails_before_parsing_test`, and `schema_invalid_structured_output_fails_agent_step_clearly_test`. Assert the expected failure code, failed step ID, failed artifact status, structured-output outcome `error`, and diagnostic text.

33. In `test/workflow_run_test.gleam`, add optional-output tests: `optional_missing_structured_output_succeeds_without_artifact_test`, `optional_blank_structured_output_succeeds_without_artifact_test`, `optional_valid_structured_output_writes_artifact_test`, and `optional_invalid_structured_output_fails_test`. Assert absent cases have status `absent`, no metadata path, and no structured file write.

34. In `test/workflow_run_test.gleam`, add `redacted_structured_payload_is_retained_test`: fake an agent final response containing `token-123`, configure secrets to include `token-123`, execute the workflow, read the structured artifact file, and assert the file content and template locals do not contain `token-123`.

35. In `test/workflow_run_test.gleam`, add `structured_artifact_write_failure_fails_step_without_metadata_test`: use a fake `workflow_checkpoint.Writer` whose `write_structured_output_artifact` returns `CheckpointArtifactFailed("structured write failed")`. Assert the step fails with `structured_output_artifact_write_failed`, no successful structured metadata is present, and the workflow failure reason mentions the code and step ID.

36. In `test/workflow_run_test.gleam`, add `structured_artifact_metadata_available_to_downstream_template_test`: use a DAG with `review_json` followed by a dependent agent step whose inline prompt contains `{{ steps.review_json.structured_output.ref }}` and `{{ steps.review_json.structured_output.path }}`. Have the fake first agent return valid JSON and have the fake second agent send its rendered prompt to the test subject, following existing test patterns. Assert the rendered prompt contains the expected ref and a path that can be read. Do not assert an absolute local prefix.

37. In `test/workflow_run_test.gleam`, add `workflow_without_structured_output_behaves_unchanged_test`: execute an agent workflow without `structured_output` using the existing fake success response. Assert the artifact succeeds, `final_response` remains `Some("response:<prompt>")` in the current test style, and `structured_output.status` in template locals is `not_configured` with no metadata path.

38. Edit `src/scherzo/workflow_run.gleam` to update the `AgentStep` pattern and route `AgentStep(_, None)` through the existing no-structured-output branch unchanged.

39. Still in `src/scherzo/workflow_run.gleam`, add the `AgentStep(_, Some(spec))` success path: validate the final response, handle `StructuredOutputAbsent`, write `StructuredOutputPresent` through `dependencies.checkpoint.write_structured_output_artifact`, attach metadata on success, and create a failed artifact on writer failure.

40. Still in `src/scherzo/workflow_run.gleam`, add the validation-error path: convert each `StructuredOutputError` to the stable failure code and message, create a failed step artifact with structured-output outcome `error`, and let existing scheduler failure handling apply.

41. Run `direnv exec . gleam test`. After Milestone 5, expect all workflow-run structured-output tests to pass.

42. Update `src/scherzo/workflow_fingerprint.gleam` and `test/workflow_fingerprint_test.gleam` if fingerprint compilation or tests show the structured-output spec is not represented. Add a test that two otherwise identical workflows have different fingerprints when one declares required structured output and the other does not.

43. Search for production workflow YAML files changed in this implementation. If any production workflow file outside tests declares `structured_output`, remove that adoption from this plan's change set and keep only test fixtures.

44. Run the full test suite:

       direnv exec . gleam test

   Expect all tests to pass.

45. Run the production lint gates:

       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

   Expect both commands to pass. If `direnv exec .` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands.

46. Update the Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective sections of this ExecPlan with implementation results and any design adjustments.

47. Commit at the end of each green milestone. Suggested commit map: Milestone 1 parser and type changes; Milestone 2 validator and redaction; Milestone 3 checkpoint writer and artifact-store support; Milestone 4 step-artifact outcomes and template locals; Milestone 5 workflow-run integration; Milestone 6 cleanup, compatibility, fingerprinting, and validation fixes. Each commit should be made only after `direnv exec . gleam test` passes for the relevant milestone or after the full validation commands pass for the final milestone.

## Testing and Falsifiability

The feature is falsified if any configured structured-output step can complete successfully without the correct outcome state, if invalid configured output is silently accepted, if a retained structured artifact contains an unredacted configured secret, if downstream templates cannot access artifact metadata after a successful structured-output step, if optional absence cannot be distinguished from an unconfigured step, or if an existing workflow without structured output changes behavior.

Add exact tests as follows.

In `test/workflow_dag_test.gleam`, test parser acceptance, defaults, and rejection. The valid parser tests must assert the parsed spec fields, not merely that parsing succeeds. Defaulting tests must cover omitted `format`, `artifact_name`, `required`, `schema.type`, and `schema.required`. Rejection tests must assert stable error codes for command-step usage, unsupported format, invalid artifact name, unsupported schema type, malformed required-key lists, and non-string required keys.

In `test/structured_output_test.gleam`, unit-test the pure validator. Use these inputs: `Some("{\"summary\":\"ok\",\"findings\":[]}")` with required keys `summary` and `findings` returns `StructuredOutputPresent`; `Some("not json")` returns `structured_output_invalid_json`; `None` and `Some("   ")` return `structured_output_missing` when required; a truncated flag returns `structured_output_truncated`; `Some("[]")` returns `structured_output_schema_invalid` for object schema; and `Some("{\"summary\":\"ok\"}")` returns `structured_output_schema_invalid` naming `findings`. For `required: false`, `None` and blank return `StructuredOutputAbsent`, valid JSON returns `StructuredOutputPresent`, nonblank invalid JSON fails, and truncation fails. Include a redaction test proving the returned payload does not contain a fake secret from the provided secret list.

In `test/structured_artifact_store_test.gleam`, test retained artifact writing and reading. Write the valid redacted payload through the new artifact-store function, assert the reference path exactly, assert checksum and byte count are set, read the file from the returned path, and assert the wrapper fields and payload are present. Include a test or assertion that the artifact file does not contain the raw fake secret from the validator redaction test.

In the local artifact tests, test that a valid structured-output wrapper is classified as a workflow artifact and that a corrupt or unreferenced structured-output wrapper is retained conservatively. This proves cleanup does not delete evidence after a partial checkpoint failure.

In `test/step_artifact_test.gleam`, test template locals and JSON round-trip for structured outcomes. Build a success artifact with valid metadata and assert each dotted local listed in the Plan of Work is exposed with the expected `template.Value`. Build an optional absent artifact and assert status is `absent`, path/ref/checksum/bytes are nil, and artifact name and format are available. Build an error artifact and assert status is `error` and the error text is exposed. Build an existing-style success artifact with no structured metadata and assert the status local is `not_configured` and path/ref/checksum locals are nil.

In `test/workflow_run_test.gleam`, add the five acceptance tests required by the Linear issue. First, `valid_json_final_response_becomes_retained_structured_artifact_test` proves valid JSON becomes a retained structured artifact. Second, `invalid_json_structured_output_fails_agent_step_clearly_test` proves invalid JSON fails clearly. Third, `missing_required_structured_output_fails_agent_step_clearly_test` proves missing required output fails clearly. Fourth, `structured_artifact_metadata_available_to_downstream_template_test` proves artifact path and metadata are available to downstream templates. Fifth, `workflow_without_structured_output_behaves_unchanged_test` proves existing workflows remain unchanged. Also include truncated, schema-invalid, optional missing, optional blank, optional valid, optional invalid, redacted retained payload, and structured artifact write-failure tests because those are important semantics and safety boundaries named in this plan.

The structured artifact write-failure test must inject a fake `workflow_checkpoint.Writer` whose `write_structured_output_artifact` callback returns a `CheckpointArtifactFailed` error. The expected behavior is a failed step artifact with `failure_code == Some("structured_output_artifact_write_failed")`, structured-output status `error`, no path/ref/checksum metadata, and a workflow failure reason that names the code and step ID.

All workflow-run tests must use fake `workflow_run.Dependencies` and `agent_types.WorkerSuccess` values. Do not start pi, do not call a live model, and do not use arbitrary sleeps. If a test needs to coordinate a fake worker that stays alive, use `test/test_async.gleam` barriers as described in `test/README.md`.

## Validation and Acceptance

From the repository root, run:

    direnv exec . gleam test

Acceptance requires all tests to pass, including the new parser, validator, checkpoint-writer, artifact-store, step-artifact, local-artifact, workflow-run, and fingerprint tests.

Then run:

    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

Acceptance requires both lint commands to pass without adding production `let assert`, `panic`, or `todo`. Warnings that predate the work should not be expanded; avoid unrelated refactors solely to reduce existing warning counts.

Behavioral acceptance is:

A workflow agent step with the configured YAML and final response `{"summary":"ok","findings":[]}` succeeds, writes a retained structured artifact under `.scherzo-state/artifacts/runs/<run>/<step>/attempt-<n>/structured/<artifact-name>.json`, and exposes path, ref, checksum, byte count, and status to downstream templates.

The same configured step with final response `not json` fails with `structured_output_invalid_json` and does not write successful structured artifact metadata.

The same configured step with no final response fails with `structured_output_missing` when `required: true`.

The same configured step with `required: false` and no final response succeeds, writes no structured artifact file, exposes `steps.<step>.structured_output.status` as `absent`, and exposes path/ref/checksum/bytes as nil.

A configured step with `required: false` and nonblank invalid JSON fails with `structured_output_invalid_json`.

A configured step whose JSON contains a configured fake secret writes a structured artifact whose file content and downstream template locals do not contain that raw secret.

A configured step whose structured artifact writer fails produces a failed step artifact with `structured_output_artifact_write_failed`, no bogus successful metadata, and a workflow failure reason that names the step ID.

A configured downstream prompt can render `{{ steps.review_json.structured_output.path }}` and use the rendered path to read the artifact file.

A workflow that does not declare `structured_output` has the same final response, success status, workflow summary behavior, and template locals it had before, aside from the new structured-output locals reporting `not_configured`.

No production workflow YAML file is committed with `structured_output` in this plan. Any use of the new syntax during implementation must live in tests or fixtures only.

## Rollout, Recovery, and Idempotence

Rollout is additive and opt-in. Existing workflow YAML files do not need migration because `structured_output` defaults to absent. Existing retained step artifacts remain decodable because the new `StepArtifact` structured-output outcome is optional. Existing command steps are unchanged.

This plan must not commit production workflow YAML that depends on `structured_output`. If a fixture workflow is needed for validation, keep it under tests. A later adoption plan can add `structured_output` blocks to production workflows with its own rollout gate and rollback steps.

Rollback is straightforward before any production workflow adopts the new syntax: revert the code changes and remove tests. After workflows adopt the syntax in a later plan, rollback to an older Scherzo build requires removing or commenting out `structured_output` blocks from those workflow YAML files before running the older build. Retained structured artifact files can remain in `.scherzo-state/artifacts`; conservative cleanup should retain unknown or unsupported artifacts rather than deleting them unsafely.

Recovery and retries should not need a new mechanism. If a workflow is interrupted before structured validation, existing recovery resumes the step attempt according to current session and checkpoint behavior. If validation fails or the structured artifact write fails, the attempt has a failed `StepArtifact` and existing retry policy can run another attempt. A later successful retry writes a structured artifact for its own attempt index, leaving the failed attempt's diagnostic artifact intact.

Artifact writing must be atomic and idempotent for the same attempt. Re-running a completed attempt should either produce the same file content and checksum or overwrite the same attempt-specific file atomically. Do not write structured artifacts outside `.scherzo-state/artifacts`.

If the structured artifact write succeeds but writing the step artifact or appending the step-finished checkpoint fails, recovery must not infer step success from the structured artifact file alone. The structured file is an orphan retained artifact until a later successful retry or operator cleanup policy addresses it. Local artifact classification must retain it for inspection rather than deleting it as an unknown file.

## Artifacts and Notes

Expected retained structured artifact reference shape:

    runs/run-1/review_json/attempt-0/structured/review_result.json

Expected downstream template locals after success:

    steps.review_json.structured_output.status = "valid"
    steps.review_json.structured_output.artifact_name = "review_result"
    steps.review_json.structured_output.format = "json"
    steps.review_json.structured_output.ref = "runs/run-1/review_json/attempt-0/structured/review_result.json"
    steps.review_json.structured_output.path = "<absolute-local-path>/.scherzo-state/artifacts/runs/run-1/review_json/attempt-0/structured/review_result.json"
    steps.review_json.structured_output.sha256 = "<sha256>"
    steps.review_json.structured_output.bytes = <positive-int>
    steps.review_json.structured_output.schema_status = "valid"
    steps.review_json.structured_output.error = ""

Expected downstream template locals after configured optional absence:

    steps.review_json.structured_output.status = "absent"
    steps.review_json.structured_output.artifact_name = "review_result"
    steps.review_json.structured_output.format = "json"
    steps.review_json.structured_output.ref = ""
    steps.review_json.structured_output.path = ""
    steps.review_json.structured_output.sha256 = ""
    steps.review_json.structured_output.bytes = ""
    steps.review_json.structured_output.schema_status = "not_applicable"
    steps.review_json.structured_output.error = ""

Expected structured failure report fragments:

    structured_output_invalid_json
    step review_json required a JSON-only final response

    structured_output_missing
    step review_json required a JSON final response but the agent returned none

    structured_output_schema_invalid
    missing required keys: findings

    structured_output_artifact_write_failed
    structured write failed

Expected redaction property:

    raw fake secret in agent JSON: token-123
    retained structured artifact file: does not contain token-123
    downstream template locals: do not contain token-123

## Interfaces and Dependencies

No new package dependency is required for the first implementation. Use `gleam/json`, `gleam/dynamic/decode`, `gleam/result`, `gleam/option`, `scherzo/log`, and existing Scherzo modules.

In `src/scherzo/workflow_dag.gleam`, the final public shape should include the structured-output types and the updated agent-step variant described in the Plan of Work.

In `src/scherzo/structured_output.gleam`, expose a validation result that gives a first-class absent success case and stable errors:

    pub type StructuredOutputValidation {
      StructuredOutputPresent(payload_json: String)
      StructuredOutputAbsent
    }

    pub fn validate_final_response(
      spec: workflow_dag.StructuredOutputSpec,
      final_response: Option(String),
      truncated: Bool,
      secrets: List(String),
    ) -> Result(StructuredOutputValidation, StructuredOutputError)

The returned present payload must already be recursively redacted. The workflow runner and artifact writer must never receive or persist an unredacted structured payload. The workflow runner should use stable error-code helpers, not message-text pattern matching.

In `src/scherzo/workflow_checkpoint.gleam`, add `StructuredOutputWrite`, `StructuredArtifactWritten`, and `Writer.write_structured_output_artifact`. The ledger writer should delegate to `artifact_store.write_structured_output_artifact`; the noop writer should return deterministic placeholder metadata; tests may inject a writer that fails this callback.

In `src/scherzo/state/artifact_store.gleam`, expose a metadata return type that can be converted into `step_artifact.StructuredOutputMetadata`. Keep artifact refs relative and validate them with the same safety rules as step artifact refs. Return the runtime path only as metadata for the current run; never store absolute paths in workflow YAML or plan text.

In `src/scherzo/step_artifact.gleam`, expose construction helpers rather than requiring `workflow_run.gleam` to know every `StepArtifact` field. The helper set must cover valid structured output, optional absent structured output, and structured-output error. This keeps structured-output failure creation consistent and reduces the chance of missing a truncation or diagnostic field.

In `src/scherzo/workflow_run.gleam`, call the structured-output code only for `AgentStep(_, Some(spec))`. For `AgentStep(_, None)`, preserve the current behavior exactly. For present validated output, write through `dependencies.checkpoint.write_structured_output_artifact`; do not instantiate `artifact_store.Store` directly in the runner.

## Open Questions and Clarifications Needed

None.
