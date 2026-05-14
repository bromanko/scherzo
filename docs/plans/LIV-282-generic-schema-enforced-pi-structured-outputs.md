# Implement generic schema-enforced Pi structured outputs

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo runs Pi as the coding agent for workflow steps. Some workflow steps require a machine-readable artifact, such as a review lane draft, rather than a prose final answer. Today, the native review path relies on a bespoke project-local Pi extension for one schema. Recent review-native runs showed why this is fragile: a terminating Pi tool call can be accepted by Pi even when its arguments are not valid for Scherzo's downstream contract, and once the tool terminates the session the model no longer has a chance to repair the arguments in the same Pi conversation.

After this change, any Scherzo workflow step that declares `structured_output.source.type: pi_tool_call` and a schema path for Pi parameters can ask Pi to register a schema-constrained terminating tool from that step's configuration. The operator-visible result is that arguments violating the same JSON Schema Scherzo will validate downstream are rejected inside the Pi session before `terminate: true`, the model receives a useful repair message, and Scherzo still validates the accepted payload deterministically before treating it as a valid artifact. The plan first proves review-native schema parity so this in-session repair claim is true for the current review-lane contract rather than merely true for a new generic wrapper. Future structured-output schemas should not require a new bespoke Pi extension.

## Problem Framing and Constraints

The operator problem is reliability. When a workflow lane needs JSON, a malformed final assistant response or a schema-invalid terminating tool call should be a recoverable model error, not an opaque workflow failure discovered only after the Pi session has ended. The concrete failures that motivated this plan were review lane drafts with non-portable path shapes, invalid enum values, lane ids inconsistent with the review lane, and extra fields in evidence targets. Those are the kinds of mistakes a model can usually repair if Pi rejects the tool call before termination and tells it what to fix.

This plan focuses on Pi tool calls because they are portable across model providers and already fit Scherzo's existing `pi_tool_call` structured-output source. Provider-native structured output features such as `response_format` or provider-specific `json_schema` settings are explicitly deferred. They may become an optimization later, but they are not the core path because support varies by provider and Scherzo already has a provider-independent Pi extension mechanism.

The implementation must preserve defense-in-depth. Pi schema validation is an early, model-facing gate. It is not the source of truth. Scherzo's existing JSON Schema validators and command validators still run after Pi accepts the tool call, and the workflow must still record validation metadata, retry structured-output failures where configured, and reject invalid artifacts even if a provider ignores part of a tool schema.

The implementation must also prevent tool-registration ambiguity. A migrated step must expose exactly one Scherzo structured-output submission tool to Pi. The legacy review-lane extension must not register `submit_review_lane_draft` when the generic structured-output spec environment variable is present, and review-native prompts must move to the generic `submit_structured_output` tool name rather than relying on two active tool names.

## Strategy Overview

Start with the current review-lane contract, not the generic extension. Compare every high-risk check in `.pi/extensions/scherzo-review-lane-draft/index.ts` with `docs/schemas/review-lane-draft.v1.schema.json`. Move schema-expressible checks into the durable JSON Schema or into repository-relative per-lane overlay schemas, and explicitly mark any remaining dynamic check as downstream-only. The initial parity target is: `schema_version` is `1`; `artifact_type` is `review_lane_draft`; `remote_mutations` is `none`; `producer` and `self_check` are objects; `lane.id` is one of `correctness`, `test-quality`, `idioms-maintainability`, or `security-performance`; each native lane step uses an overlay schema that constrains `lane.id` to that step's expected value; review-note kind, category, and severity are enums; draft-finding severity is an enum; evidence target keys are restricted; and all schema-protected artifact or source paths reject path-separator-prefixed strings, parent-directory segments, drive-letter shapes, environment-variable prefixes, and the `<absolute-local-path>` placeholder prefix. This makes Pi's raw-schema validation and Scherzo's downstream `json_schema` validator enforce the same static contract.

Add one reusable project-local Pi extension at `.pi/extensions/scherzo-structured-output/index.ts`. The extension reads a per-step tool specification JSON file whose path is provided in `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH`. If the environment variable is present and the spec is valid, the extension registers exactly the tool named by the spec, normally `submit_structured_output`. The tool's `parameters` field is the raw JSON Schema object from the spec. The tool performs no file, network, Linear, git, or workspace mutations. On successful execution it returns a compact receipt in `details` and `terminate: true`; on invalid input, Pi's tool-argument validation rejects the call before `execute` runs and before termination.

Scherzo generates the spec file under the run root for each agent step whose workflow configuration declares `structured_output.source.type: pi_tool_call` and `parameters_schema_path`. Scherzo then adds `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH` to that step's Pi environment. The generated spec records the workflow id, run id, step id, attempt index, artifact name, source tool name, schema path, and schema digest so failures are diagnosable and reproducible.

The review-native flow migrates immediately to the generic `submit_structured_output` tool name. To make coexistence safe during rollback, the existing `.pi/extensions/scherzo-review-lane-draft/index.ts` remains available but no-ops whenever `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH` is present. This means a generic step has exactly one structured-output tool, while removing `parameters_schema_path` restores the old extension path for rollback.

## Alternatives Considered

The smallest plausible alternative is to keep the existing `.pi/extensions/scherzo-review-lane-draft` extension and add more hand-written checks to it. That would address the immediate review lane mistakes, but it would preserve the pattern of creating one Pi extension per structured-output schema. The next schema would repeat the same work, and each extension would have its own drift risk between the TypeScript validator, the JSON Schema validator, prompts, and downstream Gleam validators.

Another option is to rely only on Scherzo's downstream validators and structured-output retries. That is insufficient because the model does not receive a normal tool error inside the same Pi session; it may only see a later retry prompt, and a terminating tool call can prematurely end the interactive repair loop. Downstream validators remain necessary, but they should be the final gate, not the first point at which schema-invalid arguments are noticed.

A third option is to keep the legacy `submit_review_lane_draft` tool name during migration by having the generic extension register the same name. This is rejected because the old extension already registers that name for native review scopes. Two extensions registering the same tool name would make Pi's active tool set ambiguous and could make metadata impossible to interpret. The chosen migration uses `submit_structured_output` and makes the old extension no-op when the generic spec env var is present.

Provider-native structured-output APIs are also not chosen as the primary implementation. They are attractive for providers that support them well, but Scherzo workflows run through Pi against multiple providers. A Pi tool schema is the common denominator already available to the agent loop, and the smoke milestone will prove whether Pi's tool parameter validation path can accept raw JSON Schema objects for the repository-supported Pi version.

## Risks and Countermeasures

The largest technical risk is that Pi's current raw JSON Schema support is accidental or regresses in a future supported Pi version. The second implementation milestone is therefore a smoke test that uses Pi itself, not only TypeScript object inspection, to prove that a raw JSON Schema object supplied as `tool.parameters` accepts a valid fixture and rejects invalid fixtures before `execute` and before `terminate: true`. If this proof fails, stop the implementation and do not start the broader Scherzo integration.

A schema-parity risk comes before the Pi smoke. The old review extension has schema-specific shallow checks that are not all present in `docs/schemas/review-lane-draft.v1.schema.json` today. The countermeasure is a first milestone that writes failing downstream JSON Schema tests for those gaps, updates the durable schema and per-lane overlay schemas, and records any intentionally downstream-only check in this plan before claiming in-session repair for review-native.

A duplicate-registration risk exists while the old review extension remains in the tree. The countermeasure is explicit coexistence behavior: the old extension must skip registration and report `disabled_generic_structured_output_active` from `/review-lane-draft-tool-info` whenever `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH` is set; the generic extension must refuse to register if `pi.getActiveTools()` already contains the configured tool name; and the smoke tests must assert that exactly one Scherzo structured-output tool is active for migrated review steps.

A provider-compliance risk remains even if Pi validates the schema. Some providers may ignore descriptions, loosen enum constraints before Pi sees a call, or produce malformed tool arguments. The countermeasure is to keep Scherzo's deterministic validation path unchanged: `src/scherzo/structured_output.gleam` must continue to parse the captured tool arguments, apply the baseline required-key schema, run `json_schema` validators through `src/scherzo/structured_output_json_schema.gleam`, and run command validators through `src/scherzo/structured_output_command_validator.gleam`.

A launch-plumbing risk is that the code path that writes the spec does not have the data needed by the existing `agent_step` dependency. The countermeasure is to compute the generic structured-output spec before building `workflow_run.StepContext`, add an `extra_pi_env` field to that context, and have `step_command_env(context)` append those key-value pairs. This keeps the dependency signature stable while ensuring normal runs and structured-output retries receive the same environment construction path.

A final risk is failure at step launch because the spec file is missing, malformed, or points at a schema that is not also a downstream validator. The countermeasure is to enforce `parameters_schema_path` in DAG validation, write the spec atomically under the run root before launching Pi, and fail the agent step with a clear structured-output configuration error if Scherzo cannot write or later read it. The Pi extension should also expose `/structured-output-tool-info`, analogous to the existing review extension's info command, that prints whether the generic tool is active and why not.

## Progress

- [x] (2026-05-13 00:00Z) Drafted this self-contained ExecPlan proposal for `LIV-282` under `docs/plans/`.
- [x] (2026-05-13 00:30Z) Incorporated adversarial review feedback: resolved migration coexistence, added schema-parity work, made the Pi smoke falsifiable, specified the schema-path invariant, and split implementation steps.
- [ ] Inventory bespoke review-lane extension checks against the durable JSON Schema and add parity tests.
- [ ] Update the review-lane base schema and per-lane overlay schemas so Pi and Scherzo validate the same static contract.
- [ ] Prove Pi raw JSON Schema support with a repository smoke test before broader integration work.
- [ ] Add the generic `.pi/extensions/scherzo-structured-output` extension and its spec loader.
- [ ] Add Scherzo spec generation and Pi environment handoff for configured `pi_tool_call` steps.
- [ ] Persist and extract accepted tool-call arguments and receipt metadata for generic structured outputs.
- [ ] Preserve downstream structured-output validation and retry behavior for accepted Pi tool calls.
- [ ] Migrate review-native compatibility to `submit_structured_output` without duplicate tool registration.
- [ ] Run the full validation gates and update Outcomes & Retrospective.

## Surprises & Discoveries

- Observation: Scherzo already has a typed workflow source for Pi tool-call structured outputs.
  Evidence: `src/scherzo/structured_output_source.gleam` defines `PiToolCallSource(tool_name, require_single, reject_sibling_tool_calls)` and rejects unsupported `require_single: false` or `reject_sibling_tool_calls: false` configurations.

- Observation: Scherzo already validates captured Pi tool-call arguments after the agent step.
  Evidence: `src/scherzo/structured_output.gleam` selects matching tool calls by name, rejects missing, duplicate, failed, or sibling tool calls, parses `arguments_json`, requires a JSON object, applies baseline required-key validation, and then runs configured validators.

- Observation: The current review-native extension is bespoke and schema-specific.
  Evidence: `.pi/extensions/scherzo-review-lane-draft/index.ts` registers only `submit_review_lane_draft`, contains review-lane enum lists and shallow validators, and returns `terminate: true` for accepted review lane drafts.

- Observation: The durable review-lane JSON Schema does not yet express every high-risk check in the bespoke extension.
  Evidence: `.pi/extensions/scherzo-review-lane-draft/index.ts` rejects the `<absolute-local-path>` placeholder prefix and constrains native `lane.id` values, while `docs/schemas/review-lane-draft.v1.schema.json` currently treats `lane.id` as a non-empty string and does not reject that placeholder prefix.

- Observation: There is existing smoke-test infrastructure that can be reused for generic extension checks, but it does not by itself prove Pi's raw-schema argument validation path.
  Evidence: `scripts/scherzo-review` contains `extension_schema_smoke_command`, an extension runtime fixture validator, and a Pi RPC advertisement probe for the current review-lane extension; the new smoke must additionally drive Pi through a real model-tool-call turn.

- Observation: Review-native workflow prompts live under `.scherzo/workflows/prompts/`.
  Evidence: Current prompt files include `.scherzo/workflows/prompts/review-native-correctness.md`, `.scherzo/workflows/prompts/review-native-test-quality.md`, `.scherzo/workflows/prompts/review-native-idioms-maintainability.md`, `.scherzo/workflows/prompts/review-native-security-performance.md`, and the review-native contract prompt files.

- Observation: Pi's extension documentation says a custom tool can return `terminate: true`, and termination only skips the automatic follow-up LLM call when every finalized tool result in the same batch is terminating.
  Evidence: Pi `docs/extensions.md` and `examples/extensions/structured-output.ts` describe terminating structured-output tools and custom tool registration.

## Decision Log

- Decision: Use schema-constrained terminating Pi tool calls as the primary structured-output mechanism.
  Rationale: This is provider-portable, fits Scherzo's existing `pi_tool_call` source, and gives the model an in-session repair loop when arguments are schema-invalid.
  Date: 2026-05-13

- Decision: Add one generic project-local Pi extension instead of adding a bespoke extension per schema.
  Rationale: A per-step spec file plus raw JSON Schema parameters removes review-lane-specific TypeScript drift and makes future structured-output schemas additive configuration, not new extension code.
  Date: 2026-05-13

- Decision: Generate a run-root tool spec file and pass its path with `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH`.
  Rationale: Environment variables are already used for step context, and a JSON file avoids command-line escaping problems for large schemas while leaving an auditable retained artifact.
  Date: 2026-05-13

- Decision: Keep downstream JSON Schema and command validators authoritative.
  Rationale: Pi validation improves repairability but cannot be trusted as the only gate because provider behavior can vary and Pi support may change.
  Date: 2026-05-13

- Decision: Resolve review-native migration by switching migrated workflows and prompts to `submit_structured_output` immediately and making the old review-lane extension no-op when the generic spec env var is present.
  Rationale: Registering the legacy `submit_review_lane_draft` name from both the old and generic extensions could create duplicate or ambiguous Pi tools. A distinct generic tool name plus an old-extension no-op rule keeps rollback possible without exposing two Scherzo structured-output tools in one migrated session.
  Date: 2026-05-13

- Decision: Enforce that `structured_output.source.parameters_schema_path` has a matching downstream `json_schema` validator path.
  Rationale: The same schema must gate both Pi's in-session validation and Scherzo's deterministic downstream validation; otherwise this generic mechanism would recreate the drift problem in configuration.
  Date: 2026-05-13

- Decision: Use a base review-lane schema plus repository-relative per-lane overlay schemas for native review steps.
  Rationale: Static checks such as allowed enum values and safe path shapes belong in the base durable schema, while the expected lane id for a specific native lane is step-specific and can be expressed without runtime TypeScript by using a per-lane overlay schema that downstream validation also uses.
  Date: 2026-05-13

- Decision: Prove raw-schema support with a Pi RPC smoke harness driven by a deterministic test provider.
  Rationale: Importing the extension or comparing a TypeScript schema object would not prove Pi rejects model-supplied invalid tool arguments before `execute`; a real Pi model turn with a forced invalid tool call exercises the validation path that production depends on.
  Date: 2026-05-13

- Decision: Add generic structured-output env vars to `workflow_run.StepContext` as `extra_pi_env` rather than changing the `Dependencies.agent_step` signature.
  Rationale: The code that knows the workflow step can compute the spec before creating the context, while existing fake agent dependencies can keep receiving a single context value and observe the added environment through `step_command_env(context)`.
  Date: 2026-05-13

## Outcomes & Retrospective

(To be filled at major milestones and at completion. At minimum, record whether raw JSON Schema support was proven against the repository-supported Pi version, whether review-native parity passed, whether any fallback path was needed, and whether the bespoke review-lane extension could be retired.)

## Context and Orientation

Scherzo workflow definitions declare steps. An agent step runs Pi in a prepared workspace and may declare `structured_output` when the step's result must be machine-readable. The existing structured-output implementation has two sources. `final_response` means Scherzo extracts JSON from the final assistant response. `pi_tool_call` means Scherzo extracts JSON arguments from a named Pi tool call.

The relevant production files are these:

- `src/scherzo/workflow_dag.gleam` defines workflow step and structured-output data types.
- `src/scherzo/structured_output_source.gleam` parses `structured_output.source` from workflow YAML.
- `src/scherzo/workflow_dag_validator_parser.gleam` parses structured-output validators, including repository-relative JSON Schema paths and command validators.
- `src/scherzo/workflow_fingerprint.gleam` fingerprints workflow contracts so changed structured-output schemas do not reuse stale workflow state.
- `src/scherzo/workflow_run.gleam` builds per-step context, launches agent steps through `run_attempt.run_prompt_mode_in_workspace`, and passes step environment variables through `config_types.with_pi_env(effective, step_command_env(context))`.
- `src/scherzo/local_workflow_run.gleam` has a local-run variant of the step environment in `local_step_env`.
- `src/scherzo/structured_output.gleam` validates a result artifact against the structured-output spec after the agent step completes.
- `src/scherzo/structured_output_json_schema.gleam` runs downstream JSON Schema validators.
- `src/scherzo/structured_output_command_validator.gleam` runs downstream command validators.
- `src/scherzo/structured_output_metadata.gleam` records validation metadata, including source type and source tool name.
- `src/scherzo/result_artifact.gleam` represents captured assistant output and Pi tool-call submissions.
- `src/scherzo/workflow_structured_retry.gleam` builds retry behavior for structured-output failures.
- `.pi/extensions/scherzo-review-lane-draft/index.ts` is the current review-specific Pi extension and must become a safe rollback path for migrated steps.
- `.pi/extensions/scherzo-review-lane-draft/tool-contract.v1.json` is the current review-specific extension contract.
- `.pi/extensions/scherzo-structured-output/index.ts` is the new generic extension to create.
- `scripts/scherzo-review` contains review artifact validators and the current review extension schema smoke command.
- `scripts/scherzo-structured-output-raw-schema-smoke` is the new Pi raw-schema smoke command to create.
- `docs/schemas/review-lane-draft.v1.schema.json` is the durable review lane draft JSON Schema used by downstream validation and must gain the missing base constraints.
- `docs/schemas/review-lane-draft.correctness.v1.schema.json`, `docs/schemas/review-lane-draft.test-quality.v1.schema.json`, `docs/schemas/review-lane-draft.idioms-maintainability.v1.schema.json`, and `docs/schemas/review-lane-draft.security-performance.v1.schema.json` are the per-lane overlay schemas to create for native review steps.
- `.scherzo/workflows/review-native.yml`, `.scherzo/workflows/review-native-contract-spike.yml`, `.scherzo/workflows/implementation.yaml`, and `.scherzo/workflows/execplan-implementation.yaml` are the workflow definitions whose review-lane structured-output source must be checked during migration.
- `.scherzo/workflows/prompts/review-native-correctness.md`, `.scherzo/workflows/prompts/review-native-test-quality.md`, `.scherzo/workflows/prompts/review-native-idioms-maintainability.md`, `.scherzo/workflows/prompts/review-native-security-performance.md`, `.scherzo/workflows/prompts/review-native-contract-valid.md`, `.scherzo/workflows/prompts/review-native-contract-malformed.md`, and `.scherzo/workflows/prompts/review-native-contract-failed.md` are the prompt files that must name the configured generic tool after migration.

The important tests to extend or use as examples are these:

- `test/structured_output_test.gleam` covers final-response and Pi-tool-call structured-output validation.
- `test/workflow_run_test.gleam` covers workflow execution and structured-output metadata behavior.
- `test/review_native_workflow_test.gleam` covers native review workflow expectations around `submit_review_lane_draft` today and must move to `submit_structured_output` for migrated steps.
- `test/structured_output_json_schema_test.gleam` covers JSON Schema validator behavior and review-lane fixtures.
- `test/structured_output_tool_spec_test.gleam` should be added for generic spec generation and schema-path invariants.
- `test/workflow_fingerprint_test.gleam` ensures workflow contract changes affect fingerprints.
- `test/fixtures/structured_output/pi_raw_schema/` should hold raw-schema smoke specs, payload fixtures, and the deterministic Pi test provider.

A Pi extension is a TypeScript module under `.pi/extensions/` with a default export that receives Pi's `ExtensionAPI`. It can call `pi.registerTool()` to add a tool. A tool definition includes `name`, `description`, `parameters`, and `execute`. Returning `terminate: true` from `execute` tells Pi not to perform an automatic follow-up LLM turn after the tool batch, provided every finalized tool result in the same batch also terminates. Pi RPC mode is a process-integration mode where a script sends JSONL prompts to `pi --mode rpc`; this plan uses it to run deterministic smoke checks without involving an external model provider.

## Preconditions and Verified Facts

From the current repository state, the working copy was clean before this plan was written. Source-control inspection must use:

    $SCHERZO_WORKSPACE_DRIVER status --human

The repository uses direnv for its expected toolchain. If `direnv exec . <command>` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .` from the repository root, and retry the command through direnv. Treat an unapproved `.envrc` as environment setup, not a code failure.

The existing production lint gates are:

    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

The standard test and format gates are:

    direnv exec . gleam test
    direnv exec . gleam format --check src test

The plan validation command for this file is:

    scripts/scherzo-execplan validate docs/plans/LIV-282-generic-schema-enforced-pi-structured-outputs.md

The existing Pi review-lane extension imports Pi extension APIs from packages that have appeared under both `@mariozechner/...` and `@earendil-works/...` names in this repository and Pi installation. When implementing the generic extension, follow the import style that works in the current tree and preserve compatibility with the current extension smoke runtime in `scripts/scherzo-review`.

## Scope Boundaries

In scope:

- Updating `docs/schemas/review-lane-draft.v1.schema.json` and adding per-lane overlay schemas so Pi and Scherzo can share the same static review-lane contract.
- A single reusable Pi extension at `.pi/extensions/scherzo-structured-output/index.ts`.
- A safe no-op guard in `.pi/extensions/scherzo-review-lane-draft/index.ts` when `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH` is present, so rollback remains possible without duplicate tool registration.
- A per-step JSON tool spec generated under the Scherzo run root and passed to Pi with `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH`.
- Workflow parsing, DAG validation, and fingerprinting changes needed to declare the raw JSON Schema path used as Pi tool parameters for `structured_output.source.type: pi_tool_call` steps.
- Runtime handoff changes in both normal and local workflow execution paths.
- Persistence and extraction of accepted tool-call arguments and receipt metadata.
- Tests for dynamic schema registration, invalid argument rejection before termination, accepted argument extraction, sibling-tool-call rejection, structured-output retry behavior, schema-path parity, duplicate-registration prevention, and review-native compatibility.
- Migration of the current review-native `submit_review_lane_draft` flow to the generic `submit_structured_output` mechanism.

Out of scope:

- Provider-native `response_format`, provider-native `json_schema`, or model-specific structured-output APIs.
- A new bespoke Pi extension for any individual future schema.
- Removing downstream JSON Schema or command validators.
- Keeping `submit_review_lane_draft` as an active tool in migrated review-native prompts.
- Changing review lane artifact semantics beyond the minimum needed to align the durable schema with the existing high-risk bespoke checks.
- Broad refactors of workflow execution, workspace management, or Pi session persistence unrelated to structured-output tool registration.

## Milestones

Milestone 1 proves schema parity for the existing review-native contract before any generic migration. At the end, the durable review-lane base schema rejects the same high-risk static mistakes as the bespoke extension, each native review lane has a per-lane overlay schema for its expected `lane.id`, and downstream JSON Schema tests fail if the Pi parameter schema and Scherzo validator contract drift.

Milestone 2 proves the riskiest Pi assumption: Pi accepts raw JSON Schema objects in `tool.parameters` for the repository-supported Pi version. At the end, a deterministic smoke command drives Pi through a real RPC model turn with a test provider, fails if invalid arguments reach `execute`, fails if the session terminates after an invalid call, and passes only when a valid follow-up call terminates with receipt metadata. Do not proceed to later milestones until this proof passes.

Milestone 3 introduces the generic Pi extension and its spec format without wiring workflows to it. At the end, a manually supplied spec file can register exactly one tool, `/structured-output-tool-info` reports the active tool and schema digest, invalid specs fail with useful messages, duplicate active tool names are refused, and the extension remains side-effect-free.

Milestone 4 teaches Scherzo to parse, validate, fingerprint, generate, and pass the per-step tool spec for opted-in `pi_tool_call` steps. At the end, normal workflow runs, local workflow runs, and structured-output retry attempts all carry enough environment information to dynamically register the right tool without hard-coded workflow ids in the extension.

Milestone 5 completes extraction and validation. At the end, accepted generic tool-call arguments are persisted as the structured-output payload, receipt metadata is retained, sibling tool calls are rejected according to the existing `reject_sibling_tool_calls` policy, and downstream validators still decide final validity.

Milestone 6 migrates review-native compatibility. At the end, review-native workflows and prompts use `submit_structured_output`, existing review lane JSON Schema and command validators still run, the old review-lane extension is disabled when the generic spec env var is present, and compatibility tests prove that current review artifacts are accepted or rejected exactly as before for the constraints now represented in schema and validators.

Milestone 7 performs rollout hardening. At the end, missing spec files, malformed schemas, Pi raw-schema regressions, duplicate tool registration, and provider schema noncompliance produce clear diagnostics and safe retry or rollback behavior.

## Plan of Work

Begin by aligning the review-lane schema contract. In `test/structured_output_json_schema_test.gleam`, add red tests showing the current gaps: a path beginning with the `<absolute-local-path>` placeholder prefix should be rejected; a `lane.id` outside the four native lane ids should be rejected by the base schema; a correctness-lane overlay should reject `lane.id: test-quality`; and the base schema should continue to reject invalid review-note categories and extra evidence target keys. Then update `docs/schemas/review-lane-draft.v1.schema.json` so `RepoRelativePath` also rejects the `<absolute-local-path>` prefix and `ReviewLaneMetadata.id` is an enum of the four native lane ids. Add four overlay schemas under `docs/schemas/` named for `correctness`, `test-quality`, `idioms-maintainability`, and `security-performance`; each overlay should combine the base schema with a `const` constraint for that lane id. The overlay schemas are repository files, not generated run artifacts, so they can also be used by downstream `json_schema` validators.

The initial parity inventory is as follows. `schema_version`, `artifact_type`, `remote_mutations`, `producer`, `self_check`, review-note kind/category/severity enums, draft-finding severity enum, evidence target allowed keys, and most repository-relative path checks already belong in the durable schema; the implementation should add tests before changing them to prevent regressions. The `<absolute-local-path>` placeholder prefix and base `lane.id` enum must move into `docs/schemas/review-lane-draft.v1.schema.json`. The old extension's `expectedLaneIdForStepId` behavior must move into the four per-lane overlay schemas for native review lanes. The contract-spike workflow may use the base schema when it intentionally exercises multiple lane ids. Human guidance such as which example path to use belongs in `.scherzo/workflows/prompts/*.md`, not in TypeScript validation.

Next add the raw-schema smoke harness. Create `test/fixtures/structured_output/pi_raw_schema/` with a valid review-lane payload, invalid payloads for the placeholder path, invalid note category, extra evidence target field, and wrong per-lane `lane.id`, a generated-spec fixture pointing at the correctness overlay schema, and a deterministic Pi provider extension named `smoke_provider.ts`. The provider should implement `streamSimple` and emit an invalid `submit_structured_output` tool call on its first model turn. If Pi rejects that call, the provider should observe a tool error in the next context and emit a valid `submit_structured_output` tool call. If the provider observes a successful tool result for the invalid call, or if Pi terminates before the valid call, the smoke fails. The wrapper script `scripts/scherzo-structured-output-raw-schema-smoke` should first run the targeted schema-parity test so a schema-contract gap fails as `schema_contract_failed`; then it should launch Pi in RPC mode with `--no-builtin-tools`, the generic extension, and the smoke provider, send one prompt, and parse the JSONL transcript to print the required success line.

Add `src/scherzo/structured_output_tool_spec.gleam` to represent and write the per-step tool spec. It should be independent of Pi process launch so tests can validate the JSON without starting Pi. The spec must include:

    schema_version: 1
    artifact_type: scherzo_structured_output_tool_spec
    workflow_id: <workflow id>
    run_id: <run id>
    step_id: <step id>
    attempt_index: <attempt index>
    artifact_name: <structured output artifact name>
    tool_name: <Pi tool name>
    label: <human readable label>
    description: <tool description>
    prompt_snippet: <one-line prompt snippet>
    prompt_guidelines: <list of tool-specific prompt guidelines>
    parameters_schema_path: <repository-relative JSON Schema path>
    parameters_schema_sha256: <digest of the schema file>
    parameters_schema: <raw JSON Schema object>
    require_single: true
    reject_sibling_tool_calls: true
    terminate: true

The schema path must be repository-relative and must not contain parent-directory traversal, path-separator-prefixed absolute shapes, environment-variable prefixes, or drive-letter shapes. The raw schema object must be loaded from the same repository-relative file that a downstream `json_schema` validator uses. A `pi_tool_call` source with `parameters_schema_path` but no matching downstream `json_schema` validator is invalid workflow configuration.

Extend `src/scherzo/structured_output_source.gleam` so `PiToolCallSource` records a tool-parameter schema path for generic registration. Use the field name `parameters_schema_path` under `structured_output.source`. Keep the existing `tool_name`, `require_single`, and `reject_sibling_tool_calls` fields. For backwards compatibility during rollback, allow older `pi_tool_call` declarations without `parameters_schema_path` to mean "Scherzo expects some other extension to register this tool". New generic workflows must set `parameters_schema_path`.

Enforce the schema-path invariant in `src/scherzo/workflow_dag_validator_parser.gleam` or the nearest existing DAG-validation module. Normalize repository-relative paths before comparison. When `structured_output.source.parameters_schema_path` is present, require at least one validator with `type: json_schema` and the same normalized `path`. Reject missing validators with error code `structured_output_parameters_schema_missing_json_schema_validator`. Reject mismatched paths with `structured_output_parameters_schema_path_mismatch`. Reject absolute, traversal, environment-variable, or drive-letter schema paths with `structured_output_parameters_schema_path_invalid`. Add tests for all five cases and for the matching overlay-schema case.

Update `src/scherzo/workflow_fingerprint.gleam` and `test/workflow_fingerprint_test.gleam` so changing `parameters_schema_path`, the tool name, `require_single`, `reject_sibling_tool_calls`, or the downstream `json_schema` validator path changes the workflow fingerprint. This matters because changing the registered tool schema changes the contract of a workflow step.

Close the normal-run environment plumbing by extending `workflow_run.StepContext` with an `extra_pi_env: List(#(String, String))` field or the repository's equivalent key-value environment type. The code path that has both the `WorkflowStep` and the structured-output spec should call `structured_output_tool_spec.for_step`, write the spec under `artifacts/structured-output-specs/<step-id>-attempt-<attempt-index>.json`, and populate `extra_pi_env` with `#("SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH", spec_path_for_pi)`. Then update `step_command_env(context)` so it appends `extra_pi_env` before `config_types.with_pi_env(effective, step_command_env(context))` is called. This keeps `Dependencies.agent_step` receiving the same `StepContext` value instead of requiring a signature change across every fake dependency.

Make the same environment handoff in `src/scherzo/local_workflow_run.gleam`. If local execution builds a separate context type, add the same `extra_pi_env` concept there or factor shared environment construction into a helper used by both normal and local runs. Structured-output retry attempts must recompute and rewrite the spec for the retry attempt index through the same helper; do not rely on a spec path retained from a previous failed attempt.

Create `.pi/extensions/scherzo-structured-output/index.ts`. On load, it should read `process.env.SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH`. If the variable is absent, it should register no structured-output tool and should expose `/structured-output-tool-info` with status `disabled_missing_spec_env`. If present, it should read and parse the JSON file, validate the spec shape, validate the tool name with the same lowercase name policy Scherzo accepts, require `terminate: true`, require `require_single: true`, require `reject_sibling_tool_calls: true`, require a JSON object for `parameters_schema`, and check `pi.getActiveTools()` before registration. If the configured tool name is already active, it should not register and `/structured-output-tool-info` should report `duplicate_tool_name` with the tool name.

The generic tool should use the raw `parameters_schema` object directly as `parameters`; do not convert it to TypeBox and do not copy review-lane enum lists into TypeScript. Its `description`, `promptSnippet`, and `promptGuidelines` should come from the spec. Its `execute` function should only return:

    content: a short text receipt naming the artifact and tool
    details: receipt metadata with artifact_type, tool_name, artifact_name, workflow_id, run_id, step_id, attempt_index, parameters_schema_sha256, remote_mutations: none
    terminate: true

It must not write the payload to disk, call external services, mutate files, update Linear, or inspect git. It may check that the final `params` value is an object and throw a clear error if not, but schema-specific validation must come from Pi's tool-argument validation and Scherzo's downstream validators.

Update `.pi/extensions/scherzo-review-lane-draft/index.ts` so `shouldRegisterReviewLaneDraftTool` returns false when `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH` is present. Its `/review-lane-draft-tool-info` command should report `disabled_generic_structured_output_active` in that case. This change is a safety guard, not the generic path; do not add new schema-specific checks there.

Extend `src/scherzo/result_artifact.gleam` if needed so captured Pi tool calls retain success receipt details in addition to name, arguments JSON, status, and sibling count. The existing `ToolCallSubmission` value observed in tests currently carries name, arguments JSON, status, and sibling count. If receipt details are not already available elsewhere, add an optional `details_json` or equivalent field, update decoders/encoders, and update tests. This receipt is metadata only; the structured-output payload remains the tool arguments.

Update `src/scherzo/structured_output.gleam` only as needed to preserve current validation while handling the extended tool-call submission type. It must continue to reject missing tool calls, wrong tool names, duplicate matching calls, failed tool statuses, non-object arguments, and sibling tool calls when `reject_sibling_tool_calls` is true. It must continue to run the baseline schema and every configured validator after parsing the tool arguments.

Update `src/scherzo/structured_output_metadata.gleam` so metadata for a generic Pi tool call includes source type, source tool name, schema path, schema digest, and receipt summary when present. Do not include full unredacted payloads in metadata beyond the existing structured-output artifact storage path.

Update review-native configuration and prompts. The target state is that review-native steps declare `structured_output.source.type: pi_tool_call`, `tool_name: submit_structured_output`, and `parameters_schema_path` pointing at the correct base or per-lane review schema, while retaining a downstream `json_schema` validator for the exact same path and the existing review lane command validator. Update `.scherzo/workflows/review-native.yml`, `.scherzo/workflows/review-native-contract-spike.yml`, `.scherzo/workflows/implementation.yaml`, and `.scherzo/workflows/execplan-implementation.yaml` wherever they define native review lane steps. Update `.scherzo/workflows/prompts/review-native-*.md` so they instruct the model to call the configured generic tool name and to use schema-valid review lane values. Avoid copying hard-coded schema details into the generic extension.

After parity passes, leave `.pi/extensions/scherzo-review-lane-draft` as a documented rollback shim unless maintainers explicitly remove all remaining references in the same implementation. If it remains, add a comment in its directory and tests proving generic registration is preferred when `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH` is set. Do not use the old extension for new schemas.

## Concrete Steps

1. From the repository root, verify the working copy:

       $SCHERZO_WORKSPACE_DRIVER status --human

   Expect a clean or intentionally understood working copy before implementation starts.

2. Run the existing test and lint baseline before edits:

       direnv exec . gleam test
       direnv exec . gleam format --check src test
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

   If `.envrc` is blocked, run `direnv allow .` after inspecting `.envrc`, then retry.

3. In `test/structured_output_json_schema_test.gleam`, add a red test named `review_lane_base_schema_rejects_placeholder_path_test`. Use a review-lane fixture whose `input_refs[0].path` begins with `<absolute-local-path>` and assert that validating against `docs/schemas/review-lane-draft.v1.schema.json` returns a JSON Schema rejection. Run the targeted test and expect it to fail before the schema change.

4. In `test/structured_output_json_schema_test.gleam`, add a red test named `review_lane_base_schema_rejects_unknown_lane_id_test`. Set `lane.id` to a non-empty value outside `correctness`, `test-quality`, `idioms-maintainability`, and `security-performance`; assert rejection against `docs/schemas/review-lane-draft.v1.schema.json`.

5. Update `docs/schemas/review-lane-draft.v1.schema.json`. Add the `<absolute-local-path>` placeholder prefix to the `RepoRelativePath` forbidden patterns, and change `ReviewLaneMetadata.properties.id` from a generic non-empty string to an enum of the four native lane ids.

6. Run the targeted JSON Schema tests:

       direnv exec . gleam test --target erlang --filter review_lane_base_schema

   Expect the placeholder-path and unknown-lane-id tests to pass, along with the existing review-lane schema tests.

7. Add `docs/schemas/review-lane-draft.correctness.v1.schema.json` as an overlay that references `docs/schemas/review-lane-draft.v1.schema.json` and constrains `lane.id` to `correctness`.

8. Add `docs/schemas/review-lane-draft.test-quality.v1.schema.json` as an overlay that references the base schema and constrains `lane.id` to `test-quality`.

9. Add `docs/schemas/review-lane-draft.idioms-maintainability.v1.schema.json` as an overlay that references the base schema and constrains `lane.id` to `idioms-maintainability`.

10. Add `docs/schemas/review-lane-draft.security-performance.v1.schema.json` as an overlay that references the base schema and constrains `lane.id` to `security-performance`.

11. In `test/structured_output_json_schema_test.gleam`, add overlay tests. Validate a correctness fixture against the correctness overlay and assert success; then validate the same fixture with `lane.id: test-quality` against the correctness overlay and assert rejection. Repeat one positive assertion for each other overlay schema.

12. Run the targeted overlay tests:

       direnv exec . gleam test --target erlang --filter review_lane_overlay

   Expect all overlay tests to pass. Commit point: review-lane schema parity changes are green.

13. Create `test/fixtures/structured_output/pi_raw_schema/valid-review-lane.arguments.json` using a minimal correctness-lane draft that passes the correctness overlay and includes `remote_mutations: none`.

14. Create invalid raw-schema fixtures in `test/fixtures/structured_output/pi_raw_schema/`: `invalid-placeholder-path.arguments.json`, `invalid-review-note-category.arguments.json`, `invalid-evidence-target-field.arguments.json`, and `invalid-correctness-lane-id.arguments.json`. Each fixture should differ from the valid fixture by one invalid field.

15. Add `test/fixtures/structured_output/pi_raw_schema/spec.correctness.json` with the generic spec shape for `artifact_name: review_lane_draft`, `tool_name: submit_structured_output`, and `parameters_schema_path: docs/schemas/review-lane-draft.correctness.v1.schema.json`. The implementation may generate this file in the script instead, but the fixture directory must contain a stable expected JSON example for review.

16. Add `test/fixtures/structured_output/pi_raw_schema/smoke_provider.ts`. Register a test-only provider named `scherzo-raw-schema-smoke` with model `scherzo-raw-schema-smoke`. Its first `streamSimple` response must emit a `submit_structured_output` tool call with one invalid fixture. If the next model context contains a tool error for that call, emit a valid `submit_structured_output` tool call. If the next context contains a successful tool result for the invalid call, emit final text beginning `RAW_SCHEMA_SMOKE_INVALID_REACHED_EXECUTE` so the wrapper fails.

17. Add `scripts/scherzo-structured-output-raw-schema-smoke`. The script should run `direnv exec . gleam test --target erlang --filter review_lane_overlay` first and print `STRUCTURED_OUTPUT_RAW_SCHEMA_SMOKE schema_contract=failed` if those schema tests fail. Then it should set `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH` to the generated smoke spec path, launch Pi in RPC mode with the smoke provider and generic extension, and send one prompt such as `run structured output raw schema smoke`.

18. The smoke script's Pi command should be shaped like this, adjusted only for the repository-supported Pi CLI flag spelling:

       pi --mode rpc --provider scherzo-raw-schema-smoke --model scherzo-raw-schema-smoke -e test/fixtures/structured_output/pi_raw_schema/smoke_provider.ts -e .pi/extensions/scherzo-structured-output/index.ts --no-builtin-tools --no-skills --no-prompt-templates --no-themes --no-context-files --no-session

   The script must parse the JSONL transcript and fail if the invalid tool call returns a successful receipt, if Pi exits before the valid call, if `/structured-output-tool-info` does not report one active generic tool, or if the valid call lacks `terminate: true`.

19. Run the smoke command before the generic extension exists and record the expected failure as `missing_extension` or equivalent. This confirms the wrapper is wired before the implementation makes it pass.

20. Add `src/scherzo/structured_output_tool_spec.gleam` with pure types and helpers for repository-relative schema-path validation, loading schema JSON, computing SHA-256, constructing the spec JSON, and selecting the retained run-root-relative output path.

21. Add `test/structured_output_tool_spec_test.gleam`. Cover valid spec generation, missing schema path, path-separator-prefixed schema path, parent-directory traversal, environment-variable-prefixed schema path, drive-letter schema path, malformed schema JSON, digest stability, and the retained path `artifacts/structured-output-specs/<step-id>-attempt-<attempt-index>.json`.

22. Extend `src/scherzo/structured_output_source.gleam` so `PiToolCallSource` carries `parameters_schema_path: Option(String)`. Update existing tests to prove old declarations without the field still parse during rollback.

23. In `src/scherzo/workflow_dag_validator_parser.gleam`, enforce the matching downstream validator invariant. Add tests that a `parameters_schema_path` without a `json_schema` validator fails with `structured_output_parameters_schema_missing_json_schema_validator`, a mismatched validator path fails with `structured_output_parameters_schema_path_mismatch`, and a matching overlay path succeeds.

24. Extend `src/scherzo/workflow_fingerprint.gleam` and `test/workflow_fingerprint_test.gleam` so changing `tool_name`, `parameters_schema_path`, `require_single`, `reject_sibling_tool_calls`, or the matching downstream validator path changes the fingerprint.

25. Create `.pi/extensions/scherzo-structured-output/index.ts` with exported helpers `loadSpecFromPath`, `validateSpec`, and `createStructuredOutputTool`. Keep the module side-effect-free except for registering the tool and command through Pi.

26. In `.pi/extensions/scherzo-structured-output/index.ts`, implement `/structured-output-tool-info`. It must print a line prefixed `SCHERZO_STRUCTURED_OUTPUT_TOOL_ADVERTISED=`. Active output includes `status: active`, `tool_name`, `artifact_name`, `schema_sha256`, and `active_structured_output_tool_count: 1`; inactive output names statuses such as `disabled_missing_spec_env`, `missing_spec_file`, `invalid_spec`, or `duplicate_tool_name`.

27. Run `direnv exec . scripts/scherzo-structured-output-raw-schema-smoke`. Expect the success line:

       STRUCTURED_OUTPUT_RAW_SCHEMA_SMOKE status=passed pi_version=<version> invalid=rejected_before_execute valid=accepted terminate=true

   Stop the implementation if this fails for any reason other than a known schema-contract test failure that is fixed in the same milestone.

28. Update `.pi/extensions/scherzo-review-lane-draft/index.ts` so it does not register `submit_review_lane_draft` when `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH` is set. Add or update its info-command test or smoke check to expect `disabled_generic_structured_output_active` in that environment.

29. Extend `workflow_run.StepContext` in `src/scherzo/workflow_run.gleam` with `extra_pi_env`. Update all constructors in `src/scherzo/workflow_run.gleam` and tests to initialize it to an empty list when no generic structured output is configured.

30. In the `src/scherzo/workflow_run.gleam` code path that has the current `WorkflowStep`, structured-output spec, run id, step id, and attempt index, call `structured_output_tool_spec.for_step` and `structured_output_tool_spec.write` before creating the final `StepContext`. Add `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH` to `extra_pi_env` for generic `pi_tool_call` steps only.

31. Update `step_command_env(context)` in `src/scherzo/workflow_run.gleam` so it appends `extra_pi_env` to the standard step environment. Add a `test/workflow_run_test.gleam` case where a fake agent dependency observes the env var for a generic step and does not observe it for a legacy `pi_tool_call` step without `parameters_schema_path`.

32. Apply the same environment helper in `src/scherzo/local_workflow_run.gleam`. Add a local-run test proving the generated spec path is present for a generic local step.

33. Add a structured-output retry test for `src/scherzo/workflow_structured_retry.gleam` behavior. The retry prompt must name `submit_structured_output`, must not ask for final assistant JSON, and must recompute a spec path for the retry attempt index.

34. Extend `src/scherzo/result_artifact.gleam`, `src/scherzo/step_artifact.gleam`, and related JSON encoders/decoders only as needed to retain receipt metadata. Add backwards-compatibility tests for old tool-call submissions without receipt details.

35. Update `src/scherzo/structured_output.gleam` tests in `test/structured_output_test.gleam` for accepted generic tool-call arguments, missing tool call, wrong tool name, duplicate matching calls, failed status, non-object arguments, sibling rejection, final-response JSON ignored when source is `pi_tool_call`, and downstream JSON Schema rejection after Pi acceptance.

36. Update `src/scherzo/structured_output_metadata.gleam` and `test/workflow_run_test.gleam` so metadata records source type `pi_tool_call`, source tool name `submit_structured_output`, schema path, schema digest, validation status, and receipt summary without duplicating the full payload.

37. Update `.scherzo/workflows/review-native.yml`. For each native lane step, set `tool_name: submit_structured_output`, set `parameters_schema_path` to the matching per-lane overlay schema, and ensure a downstream `json_schema` validator uses the exact same path.

38. Update `.scherzo/workflows/implementation.yaml` and `.scherzo/workflows/execplan-implementation.yaml` wherever they define native review lane steps. Use the same generic tool name and matching overlay schema rule.

39. Update `.scherzo/workflows/review-native-contract-spike.yml`. Use `tool_name: submit_structured_output`; use the base schema for contract-spike steps unless a step intentionally needs a per-lane overlay; ensure the downstream `json_schema` validator path matches `parameters_schema_path`.

40. Update `.scherzo/workflows/prompts/review-native-correctness.md`, `.scherzo/workflows/prompts/review-native-test-quality.md`, `.scherzo/workflows/prompts/review-native-idioms-maintainability.md`, `.scherzo/workflows/prompts/review-native-security-performance.md`, `.scherzo/workflows/prompts/review-native-contract-valid.md`, `.scherzo/workflows/prompts/review-native-contract-malformed.md`, and `.scherzo/workflows/prompts/review-native-contract-failed.md` so they instruct the model to call `submit_structured_output` exactly once with a schema-valid `review_lane_draft` object.

41. Update `test/review_native_workflow_test.gleam` so migrated review-native expectations refer to `submit_structured_output`, assert matching `parameters_schema_path` and downstream validator path, assert the old extension is disabled when the generic spec env var is present, and preserve rejection cases for placeholder paths, invalid note categories, wrong per-lane `lane.id`, and extra evidence target fields.

42. Update `scripts/scherzo-review` smoke validation that currently expects `submit_review_lane_draft` for native review lanes. During the compatibility window, it should accept `submit_structured_output` for workflows with `parameters_schema_path` and still accept the legacy name only for workflows without that field.

43. Run targeted validation after each milestone. At minimum, after milestones 1, 2, 4, 5, and 6, run:

       direnv exec . gleam test
       direnv exec . scripts/scherzo-structured-output-raw-schema-smoke

44. Commit at logical green points. Suggested commit map: one commit for review-lane schema parity, one for the raw-schema smoke proof, one for the generic extension and spec writer, one for workflow handoff and extraction, one for review-native migration, and one for cleanup or rollback-shim documentation. Each commit should be made only after relevant tests pass.

45. Run final validation from the repository root:

       direnv exec . gleam test
       direnv exec . gleam format --check src test
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint
       direnv exec . scripts/scherzo-structured-output-raw-schema-smoke

46. Update this ExecPlan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective with final results before marking the work complete.

## Testing and Falsifiability

The schema-parity tests are the first falsifiability gate. `test/structured_output_json_schema_test.gleam` must prove that the review-lane base schema rejects the `<absolute-local-path>` placeholder prefix, unknown lane ids, invalid review-note categories, invalid severities, extra evidence target fields, and unsafe artifact paths. It must also prove that each per-lane overlay accepts its own lane id and rejects a different native lane id. If these tests fail, the generic extension must not claim review-native in-session repair for those cases.

The raw-schema smoke is the second falsifiability gate for the core Pi assumption. It must exercise Pi's actual tool argument validation path with raw JSON Schema in `tool.parameters`. A test that only imports the extension and compares the `parameters` object is not enough. The smoke must prove all of these facts: the valid review lane fixture is accepted; the placeholder-path fixture is rejected; the invalid review note category fixture is rejected; the extra evidence target field fixture is rejected; the wrong per-lane lane-id fixture is rejected; invalid calls do not run `execute`; invalid calls do not terminate the Pi session; valid calls return `terminate: true` and receipt metadata; and `/structured-output-tool-info` reports exactly one active generic structured-output tool.

The smoke mechanism is concrete. `scripts/scherzo-structured-output-raw-schema-smoke` first runs the targeted schema-parity test and reports `schema_contract_failed` if the chosen schema does not reject the invalid fixtures. It then launches Pi in RPC mode with `test/fixtures/structured_output/pi_raw_schema/smoke_provider.ts` and `.pi/extensions/scherzo-structured-output/index.ts`. The provider emits an invalid tool call first and a valid tool call only after Pi returns a tool error to the model context. If invalid arguments reach `execute`, the generic tool returns a receipt and terminates, so the wrapper observes no valid follow-up call and fails with `invalid_reached_execute`. If Pi rejects the raw JSON Schema object at registration time, the info command reports `invalid_spec` or Pi exits before the first prompt, and the wrapper fails with `raw_schema_not_supported`.

`test/structured_output_tool_spec_test.gleam` should assert that a spec built for artifact `review_lane_draft` and tool `submit_structured_output` contains the raw schema object, the repository-relative schema path, a stable SHA-256 digest, `terminate: true`, `require_single: true`, and `reject_sibling_tool_calls: true`. It should also assert that schema paths beginning with a path separator, containing parent segments, beginning with an environment-variable prefix, using drive-letter shapes, or beginning with `<absolute-local-path>` are rejected before any Pi launch.

DAG validation tests should assert the schema-path invariant. A generic `pi_tool_call` source with `parameters_schema_path: docs/schemas/review-lane-draft.correctness.v1.schema.json` and no downstream `json_schema` validator must fail with `structured_output_parameters_schema_missing_json_schema_validator`. The same source with a downstream validator for `docs/schemas/review-lane-draft.v1.schema.json` must fail with `structured_output_parameters_schema_path_mismatch`. The same source with a downstream validator for the correctness overlay path must pass.

`test/structured_output_test.gleam` should continue to prove downstream defense-in-depth. Add a case where the tool call status is successful and the arguments are JSON but violate the configured overlay schema; the expected result is a `structured_output_json_schema_rejected` error from the configured validator. Add a case where a matching tool call has `sibling_count: 2`; the expected error code is `structured_output_tool_call_sibling`. Add a case where the final assistant response contains valid JSON but the source is `pi_tool_call`; the expected behavior is that Scherzo ignores the final response and requires the tool call.

`test/workflow_run_test.gleam` should prove integration. Use a tiny workflow with one agent step, `structured_output.source.type: pi_tool_call`, `tool_name: submit_structured_output`, `parameters_schema_path` pointing at a fixture schema, and a downstream `json_schema` validator pointing at the same schema. Use a fake `agent_step` dependency that returns a result artifact with a successful tool call and receipt metadata. Assert that Scherzo writes or records the spec path, passes `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH` through `step_command_env`, persists the tool arguments as the structured-output payload, records source type `pi_tool_call`, records source tool name `submit_structured_output`, and marks validation passed.

`test/workflow_fingerprint_test.gleam` should include these changes: changing the tool name changes the fingerprint, changing `parameters_schema_path` changes the fingerprint, changing `require_single` or `reject_sibling_tool_calls` changes the fingerprint, and changing a downstream validator still changes the fingerprint. This catches accidental reuse of stale workflow runs when the tool schema changes.

`test/review_native_workflow_test.gleam` should preserve review-native compatibility while asserting the new migration shape. It should assert that the review lane draft schema still rejects placeholder paths, invalid note categories, wrong per-lane lane ids, and extra evidence target fields; that the workflow source is `pi_tool_call`; that the configured tool name is `submit_structured_output`; that `parameters_schema_path` equals the downstream `json_schema` validator path; and that the legacy `submit_review_lane_draft` tool is inactive when the generic spec env var is present.

The failure-mode tests should cover missing spec env, missing spec file, malformed spec JSON, malformed raw schema, duplicate tool name, unsupported `terminate: false`, unsupported `require_single: false`, unsupported `reject_sibling_tool_calls: false`, missing downstream `json_schema` validator, mismatched downstream schema path, and a provider or fake agent returning tool arguments that Pi would have rejected but Scherzo still sees. Each failure must produce a message that names the step id, artifact name, tool name when known, and the failing path or schema field when safe to display.

## Validation and Acceptance

Implementation is accepted when all of these commands pass from the repository root:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint
    direnv exec . scripts/scherzo-structured-output-raw-schema-smoke

The smoke command must include the current supported Pi version in its output. If the repository pins Pi through devenv or another toolchain file, record that version in the smoke manifest and in this plan's Surprises & Discoveries during implementation.

Behavioral acceptance is:

- The durable review-lane base schema and per-lane overlay schemas reject the high-risk mistakes previously handled by the bespoke extension's shallow validators.
- A workflow step with `structured_output.source.type: pi_tool_call`, `tool_name: submit_structured_output`, `parameters_schema_path`, and a matching downstream `json_schema` validator causes Scherzo to generate a per-step spec file and launch Pi with `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH`.
- Workflow parsing rejects a generic `pi_tool_call` source whose `parameters_schema_path` is missing a matching downstream `json_schema` validator.
- Pi advertises exactly the configured generic structured-output tool for that step and does not register `submit_review_lane_draft` at the same time.
- Invalid arguments that violate the configured raw schema are rejected before the structured-output tool returns `terminate: true`, allowing the model to repair within the same Pi session.
- A valid tool call returns a side-effect-free receipt with `remote_mutations: none` and terminates the Pi session without an extra assistant response when it is the only tool call in the batch.
- Scherzo extracts the accepted tool arguments as the structured-output payload and records source type, source tool name, schema path, schema digest, validation status, and receipt metadata.
- Scherzo still rejects accepted tool arguments if the downstream JSON Schema or command validators fail.
- Review-native workflows use `submit_structured_output` and continue to pass their existing review lane contract tests.

This plan file itself should validate with:

    scripts/scherzo-execplan validate docs/plans/LIV-282-generic-schema-enforced-pi-structured-outputs.md

## Rollout, Recovery, and Idempotence

Roll out additively but avoid ambiguous coexistence. The generic extension may be committed alongside the existing review-lane extension. Workflows opt into the generic extension by declaring `parameters_schema_path` and a matching downstream `json_schema` validator. When that env var is present at Pi launch, the old review-lane extension must report `disabled_generic_structured_output_active` and must not register `submit_review_lane_draft`. A workflow without `parameters_schema_path` should continue to behave as it does today during rollback, relying on whatever extension currently registers its named tool.

Rollback is straightforward if the migration is kept additive. To roll back a workflow, remove `parameters_schema_path`, restore the previous tool name and prompts, and launch Pi without `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH`. The old review-lane extension should not be deleted until review-native parity has passed and operators agree the rollback shim is no longer needed.

If the spec file is missing, the Pi extension should not silently proceed. It should report `missing_spec_file` through `/structured-output-tool-info`, and an agent step that required generic structured output should fail with a clear structured-output configuration error. If schema loading fails, Scherzo should fail before launching Pi when possible; if Pi discovers the failure first, the extension should throw a startup or command diagnostic that names the spec field and safe relative path.

If the generic extension sees that the configured tool name is already active, it should refuse to register and report `duplicate_tool_name`. That failure means the rollout has loaded conflicting extensions or configuration; the recovery is to remove the duplicate extension from that Pi session or choose a distinct tool name before retrying. Do not continue with two active Scherzo structured-output tools.

If Pi raw JSON Schema support regresses, the early smoke command fails. In that state, do not migrate additional workflows. Keep or restore the old review-lane extension as a rollback path while deciding whether to pin Pi, adapt the schema format, or temporarily use TypeBox generation. Any such change must be recorded in the Decision Log before implementation continues.

If a provider ignores parts of the tool schema and Pi still emits a tool call, Scherzo's downstream validators must reject the payload. That failure should use the existing structured-output retry path when the step is retryable. The retry prompt should mention the configured tool name and the validator diagnostic so the model can call the same tool again with corrected arguments.

Spec generation should be idempotent for a given workflow id, run id, step id, attempt index, tool name, and schema file contents. Re-running a step attempt may overwrite the same spec path with identical content. A new attempt index should write a new spec path so retained artifacts can distinguish attempts.

## Artifacts and Notes

Expected raw-schema smoke success transcript:

    STRUCTURED_OUTPUT_RAW_SCHEMA_SMOKE status=passed pi_version=<version> invalid=rejected_before_execute valid=accepted terminate=true

Expected raw-schema smoke schema-contract failure transcript:

    STRUCTURED_OUTPUT_RAW_SCHEMA_SMOKE schema_contract=failed failing_test=review_lane_overlay

Expected generic tool info transcript when active:

    SCHERZO_STRUCTURED_OUTPUT_TOOL_ADVERTISED={"status":"active","tool_name":"submit_structured_output","artifact_name":"review_lane_draft","schema_sha256":"<sha256>","active_structured_output_tool_count":1}

Expected generic tool info transcript when no spec env is present:

    SCHERZO_STRUCTURED_OUTPUT_TOOL_ADVERTISED={"status":"disabled_missing_spec_env"}

Expected legacy review-lane tool info transcript when the generic spec env is present:

    REVIEW_LANE_DRAFT_TOOL_ADVERTISED={"status":"disabled_generic_structured_output_active","tool_name":"submit_review_lane_draft"}

The generated spec is a retained diagnostic artifact, not the structured-output payload. The payload remains the accepted tool-call arguments extracted from Pi's result artifact. The receipt metadata proves that the extension accepted the call and returned `terminate: true`; it is not a substitute for payload validation.

## Interfaces and Dependencies

The workflow configuration interface for a native correctness lane after migration should look like this, with field names adjusted only if existing parser conventions require a different spelling:

    structured_output:
      artifact_name: review_lane_draft
      required: true
      source:
        type: pi_tool_call
        tool_name: submit_structured_output
        parameters_schema_path: docs/schemas/review-lane-draft.correctness.v1.schema.json
        require_single: true
        reject_sibling_tool_calls: true
      schema:
        required: [schema_version, artifact_type, generated_at_utc, producer, lane, input_refs, draft_findings, review_notes, evidence_requests, self_check, remote_mutations]
      validators:
        - name: review_lane_shape
          type: json_schema
          path: docs/schemas/review-lane-draft.correctness.v1.schema.json
          draft: "2020-12"
        - name: review_lane_contract
          type: command
          command: scripts/scherzo-review

The contract-spike workflow may use `docs/schemas/review-lane-draft.v1.schema.json` for both `parameters_schema_path` and the downstream `json_schema` validator when the step intentionally should not constrain a single native lane id.

In `src/scherzo/structured_output_source.gleam`, the end-state type should carry the schema path:

    pub type StructuredOutputSource {
      FinalResponseSource
      PiToolCallSource(
        tool_name: String,
        require_single: Bool,
        reject_sibling_tool_calls: Bool,
        parameters_schema_path: Option(String),
      )
    }

If adding the new field directly causes too much churn, use a nested record type, but keep the public meaning the same.

In `src/scherzo/structured_output_tool_spec.gleam`, expose pure helpers with names like:

    pub fn for_step(
      context: workflow_run.StepContext,
      spec: workflow_dag.StructuredOutputSpec,
      schema_path: String,
    ) -> Result(ToolSpec, ToolSpecError)

    pub fn write(
      tool_spec: ToolSpec,
      run_root: String,
    ) -> Result(WrittenToolSpec, ToolSpecError)

`WrittenToolSpec` should contain both the path passed in `SCHERZO_STRUCTURED_OUTPUT_TOOL_SPEC_PATH` and the retained run-root-relative path used in logs and metadata, so tests can keep logs portable.

In `src/scherzo/workflow_run.gleam`, `StepContext` should gain a field equivalent to:

    extra_pi_env: List(#(String, String))

`step_command_env(context)` should append `extra_pi_env` to the normal Scherzo step environment. If the repository uses a map or custom environment type instead of a list of tuples, use that existing type and preserve the same meaning.

In `.pi/extensions/scherzo-structured-output/index.ts`, export small pure helpers for testability:

    export function loadSpecFromPath(path: string): StructuredOutputToolSpec
    export function validateSpec(value: unknown): StructuredOutputToolSpec
    export function createStructuredOutputTool(spec: StructuredOutputToolSpec): ToolDefinition

The extension's default export should read the env var, register the tool when possible, refuse duplicate active tool names, and always register `/structured-output-tool-info`.

In `test/fixtures/structured_output/pi_raw_schema/smoke_provider.ts`, register a test-only Pi provider using `pi.registerProvider("scherzo-raw-schema-smoke", ...)` with a `streamSimple` implementation. The stream must emit a tool call event with the configured fixture arguments, not call the generic extension helper directly. This is what makes the smoke exercise Pi's real tool-call validation path.

No new runtime dependency should be added unless the raw-schema smoke proves Pi requires one. Prefer Node built-ins and Pi's existing extension dependencies. Do not add AJV or another JSON Schema validator to the extension unless the early proof fails and the Decision Log is updated to explain why Pi-native validation is insufficient.

## Open Questions and Clarifications Needed

None.
