# Structured Output Validator Specification

Status: Draft v1

Purpose: Define the normative contract for Scherzo structured-output declarations, source extraction, generic validation, retry behavior, artifact persistence, diagnostics, and workflow fingerprinting.

## Normative language

The key words `MUST`, `MUST NOT`, `REQUIRED`, `SHOULD`, `SHOULD NOT`, `RECOMMENDED`, `MAY`, and `OPTIONAL` in this document are to be interpreted as described in RFC 2119.

`Implementation-defined` means the behavior is part of Scherzo's contract, but this specification does not prescribe one universal mechanism. Scherzo MUST document the selected behavior when it relies on implementation-defined semantics.

## 1. Purpose and scope

Structured output is the workflow contract that turns an agent step's model response into a retained JSON artifact that later steps, operators, and workflow-specific scripts can consume reliably. Scherzo currently has review-native structured output, but future workflows need the same primitive for kickoff packets, alignment proposals, proof bundles, acceptance decisions, merge records, and other artifacts without adding new Scherzo runtime types for each domain.

This specification covers:

- workflow YAML structured-output declaration syntax,
- Pi tool-call source extraction for production structured output,
- baseline JSON admission checks owned by Scherzo,
- ordered generic validator execution,
- command validator process contracts,
- JSON Schema validator contracts,
- retry behavior after structured-output failure,
- artifact persistence and metadata,
- diagnostics capture, truncation, and redaction,
- command validator environment variables,
- workflow fingerprinting,
- backwards compatibility and migration, and
- conformance expectations and tests.

This specification applies to structured output on agent steps only. Command steps MAY produce files or stdout as normal command artifacts, but command steps MUST NOT declare `structured_output` under this contract.

This specification does not define new workflow domains, review synthesis semantics, kickoff workflow content, alignment proposal content, proof bundle content, acceptance decision content, Pi extension registration, operating-system sandboxing, or non-JSON structured-output formats.

**SOV-SCOPE-001:** Scherzo MUST implement structured output as a generic workflow primitive for agent steps, not as a review-native-only feature.

**SOV-SCOPE-002:** Scherzo MUST NOT allow command steps to declare `structured_output`.

**SOV-DOMAIN-001:** Scherzo MUST NOT contain domain-specific structured-output runtime validators such as `ReviewLaneDraftValidator`, `KickoffPacketValidator`, `ProofBundleValidator`, or equivalent artifact-specific runtime cases.

**SOV-DOMAIN-002:** Semantic validation for structured artifacts MUST be expressed through JSON Schema validators, command validators, or both.

**SOV-DOMAIN-003:** Pi tool-call structured output is a capture mechanism and MUST NOT be treated as the source of truth for semantic validation.

## 2. Glossary

**Structured output**: a JSON value captured from an agent step according to a `structured_output` declaration.

**Structured artifact**: the retained artifact written by Scherzo after a structured output passes source extraction, baseline admission, and every configured validator.

**Source**: the place Scherzo extracts the candidate JSON value from. Production structured output requires `pi_tool_call`.

**Final-response source**: a legacy/internal source shape retained only for historical decoding and non-production helper coverage. It is not a supported production workflow declaration.

**Pi tool-call source**: a source that captures object-valued JSON arguments from one successful Pi tool call with a configured name.

**Baseline admission**: Scherzo-owned generic checks applied after source extraction and before validators. Baseline admission includes presence, truncation, JSON parsing, top-level object requirements, and declared top-level required keys.

**Validator**: a generic declaration that runs after baseline admission. Version 1 validators are JSON Schema validators and command validators.

**JSON Schema validator**: a declarative validator that checks admitted JSON against a repository-relative JSON Schema file.

**Command validator**: a trusted workflow-declared local process that receives the admitted payload on stdin and accepts or rejects it by exit status.

**Payload rejection**: a validator result meaning the agent produced JSON, but the JSON does not satisfy the declared artifact contract. Payload rejections are eligible for structured-output retry when the output is required and retry budget remains.

**Configuration failure**: a validator result meaning Scherzo or the workflow declaration cannot run the validator correctly, such as a missing schema file, invalid schema, invalid command declaration, start failure, timeout, or validator internal error. Configuration failures are not eligible for agent retry.

**Redacted payload**: the admitted JSON value after Scherzo replaces known secret strings with redacted text.

**Workflow fingerprint**: Scherzo's canonical representation of workflow semantics used to detect changes that affect execution behavior.

## 3. Structured-output declaration schema

A structured-output declaration appears under an agent step:

```yaml
- id: produce_artifact
  kind: agent
  prompt: prompts/produce-artifact.md
  structured_output:
    artifact_name: artifact_packet
    required: true
    format: json
    source:
      type: final_response
    schema:
      type: object
      required:
        - schema_version
        - artifact_type
    validators:
      - name: artifact_shape
        type: json_schema
        path: schemas/artifact_packet.schema.json
        draft: "2020-12"
      - name: artifact_semantics
        type: command
        argv:
          - python3
          - scripts/validate-artifact
        timeout: 30s
        working_directory: repository
    validation_retries: 1
```

**SOV-DECL-001:** `structured_output` MUST be a map when present and MUST be valid only on agent steps.

**SOV-DECL-002:** `format` MAY be omitted and defaults to `json`. If present, it MUST be the string `json`. Scherzo MUST reject any other structured-output format in version 1.

**SOV-DECL-003:** `artifact_name` MAY be omitted and defaults to the step id. When present, it MUST be a non-empty stable identifier accepted by Scherzo's artifact-name validator.

**SOV-DECL-004:** `required` MAY be omitted and defaults to `true`. When present, it MUST be a boolean.

**SOV-DECL-005:** `validation_retries` MAY be omitted and defaults to `1`. Version 1 MUST accept only integer values `0` and `1`.

**SOV-DECL-006:** `source` is REQUIRED for production structured output and MUST be a map with `type: pi_tool_call`. Scherzo MUST reject omitted `source` blocks and `source.type: final_response` in parsed workflow definitions.

**SOV-DECL-007:** `schema` MAY be omitted and defaults to `{type: object, required: []}`. Version 1 `schema` is baseline admission, not JSON Schema. If present, `schema.type` MUST be `object` when provided, and `schema.required` MUST be a list of string top-level keys when provided.

**SOV-DECL-008:** `validators` MAY be omitted and defaults to an empty list. When present, it MUST be a list. Scherzo MUST preserve declaration order and MUST run validators in that order.

**SOV-DECL-009:** Each validator MAY include `name`. If omitted, Scherzo MUST assign a stable generated name for diagnostics and fingerprinting. Validator names MUST be stable identifiers and MUST be unique within one `structured_output` declaration after generated names are assigned.

**SOV-DECL-010:** A JSON Schema validator MUST declare `type: json_schema` and a repository-relative `path`. It MAY declare `draft`; version 1 MUST support `draft: "2020-12"` and MAY treat an omitted draft as `"2020-12"`.

**SOV-DECL-011:** A command validator MUST declare `type: command` and a non-empty `argv` list of strings whose first entry is a non-empty executable token. It MAY declare duration-string `timeout`, `working_directory`, and `env`. `timeout` defaults to `30s` and MUST be positive. `working_directory` defaults to `workspace` and MUST be one of `workspace`, `repository`, or `run_root` when provided. `env` MUST be a string-to-string map when provided.

**SOV-DECL-012:** A declaration MUST NOT contain both the legacy singular `validator` field and the generic `validators` field. Scherzo MUST reject that ambiguous shape.

**SOV-DECL-013:** New workflow files and examples MUST use `validators`, not the legacy singular `validator` field.

## 4. Source extraction semantics

Source extraction produces a candidate JSON document for baseline admission. Source extraction proves provenance and transport shape only; it does not prove artifact semantics.

### 4.1 Final response

`final_response` is not a production structured-output source. The semantics below document the legacy/internal shape only.

**SOV-SRC-001:** For `source.type: final_response`, Scherzo MUST use the final assistant response capture for the agent attempt.

**SOV-SRC-002:** If the final response capture is marked truncated, Scherzo MUST reject it before JSON parsing.

**SOV-SRC-003:** Scherzo MUST trim leading and trailing whitespace before parsing. If the trimmed value is empty and `required: true`, Scherzo MUST report missing structured output. If the trimmed value is empty and `required: false`, Scherzo MUST report structured output absent.

**SOV-SRC-004:** Scherzo MUST require the entire trimmed final response to parse as exactly one JSON document. Markdown fences, commentary, transcripts, or extra text MUST be rejected because they are not a single JSON document.

### 4.2 Pi tool call

A production Pi tool-call source has this shape:

```yaml
source:
  type: pi_tool_call
  tool_name: submit_artifact
  require_single: true
  reject_sibling_tool_calls: true
```

**SOV-SRC-005:** For `source.type: pi_tool_call`, `tool_name` MUST be present, MUST be a valid tool identifier, and MUST name the Pi tool whose JSON arguments are the candidate structured output.

**SOV-SRC-006:** Version 1 supports only `require_single: true` and `reject_sibling_tool_calls: true`. If either field is omitted, Scherzo MUST default it to `true`. If either field is explicitly `false`, Scherzo MUST reject the workflow declaration until broader semantics are specified.

**SOV-SRC-007:** Scherzo MUST require exactly one successful Pi tool call with the configured name. It MUST reject missing calls, only-wrong-name calls, failed calls, duplicate matching calls, and matching calls submitted with sibling tool calls in the same assistant tool-call batch.

**SOV-SRC-008:** Scherzo MUST require the matching tool call to contain JSON arguments and MUST require those arguments to parse as a JSON object. Non-object arguments MUST fail source extraction.

**SOV-SRC-009:** Pi tool-call structured output MUST be treated only as a capture and provenance mechanism. Artifact-specific semantics MUST still be validated by baseline admission, JSON Schema validators, and/or command validators.

## 5. Baseline JSON admission checks

Baseline admission runs after source extraction and before any configured validator.

**SOV-BASE-001:** Scherzo MUST run baseline admission for every present structured output before any JSON Schema or command validator.

**SOV-BASE-002:** Scherzo MUST enforce `format: json` before validators run.

**SOV-BASE-003:** Scherzo MUST reject invalid JSON, truncated capture, and missing required output with stable structured-output error codes.

**SOV-BASE-004:** When `schema.type: object` is configured or defaulted, Scherzo MUST require the top-level JSON value to be an object.

**SOV-BASE-005:** Scherzo MUST check every key listed in `schema.required` as a top-level object key. Missing top-level required keys MUST fail baseline admission.

**SOV-BASE-006:** Baseline admission MUST NOT contain artifact-specific semantics. It MUST NOT know review lanes, proof-bundle evidence rules, kickoff packet fields beyond configured top-level required keys, or any future domain-specific invariant.

**SOV-BASE-007:** Scherzo SHOULD keep both the admitted raw JSON value and a redacted JSON value in memory. The raw value MAY be used by Scherzo-owned validators such as the JSON Schema helper. Workflow-declared command validators and persisted artifacts MUST receive the redacted value.

## 6. Validator pipeline ordering

**SOV-PIPE-001:** For each present structured output, Scherzo MUST execute this pipeline in order: source extraction, baseline admission, configured validators in declaration order, structured artifact persistence.

**SOV-PIPE-002:** Scherzo MUST NOT persist a structured artifact before baseline admission and every configured validator have passed.

**SOV-PIPE-003:** Validators MUST short-circuit on first failure. Validators after a failing validator MUST NOT run for that attempt.

**SOV-PIPE-004:** JSON Schema validators MAY validate the admitted raw JSON value because they are Scherzo-owned validation mechanisms. JSON Schema diagnostics MUST NOT echo the full payload.

**SOV-PIPE-005:** Command validators MUST receive the redacted admitted JSON value on stdin. Command validators MUST NOT receive known secret strings from Scherzo through structured-output stdin.

**SOV-PIPE-006:** A structured output with no validators is accepted after source extraction and baseline admission pass.

## 7. Command validator contract

Command validators are the semantic escape hatch for checks that cannot be expressed well in JSON Schema. They are generic; the command may be review-specific, proof-specific, or workflow-specific, but Scherzo sees only a command validator declaration and a process result.

**SOV-CMD-001:** A command validator is trusted workflow code, not sandboxed code. Scherzo does not guarantee read-only filesystem or network isolation in version 1. Operators MUST only run workflows whose validator commands they trust.

**SOV-CMD-002:** Scherzo MUST start command validators without a shell. It MUST treat `argv` as an executable plus literal arguments and MUST NOT perform shell interpolation, globbing, or quoting.

**SOV-CMD-003:** `argv[0]` MUST be non-empty. `argv[0]` with a path separator MUST resolve as a repository-relative path and MUST be rejected if it is absolute or traverses outside the repository. `argv[0]` without a path separator MAY be resolved through the clean validator `PATH`.

**SOV-CMD-004:** Scherzo MUST set the process working directory according to `working_directory`: the agent step workspace for `workspace`, the repository root for `repository`, or the run root for `run_root`.

**SOV-CMD-005:** Scherzo MUST write exactly one UTF-8 JSON document followed by a newline to the validator's stdin and then close stdin. The JSON document MUST be the redacted admitted payload.

**SOV-CMD-006:** Scherzo MUST use a clean environment as defined in [environment variables](#12-environment-variables-exposed-to-command-validators). It MUST NOT inherit daemon credentials wholesale.

**SOV-CMD-007:** Exit status `0` MUST mean accepted. Exit status `1` MUST mean payload rejected and retryable when retry budget remains. Exit status `2` MUST mean validator configuration or internal error and MUST NOT be retried. Any other nonzero exit status MUST be treated as a non-retryable validator error unless a later spec version defines additional meanings.

**SOV-CMD-008:** Command start failure MUST be a non-retryable configuration failure.

**SOV-CMD-009:** Command timeout MUST be a non-retryable validator error. Scherzo MUST stop accepting further output, terminate the child process or closest available process abstraction, and reap or otherwise clean up the child before returning the failure.

**SOV-CMD-010:** Scherzo MUST capture stdout and stderr separately while the command runs and MUST drain both streams without deadlock. Failure diagnostics MAY summarize stdout and stderr, but success artifacts MUST NOT persist command stdout or stderr by default.

**SOV-CMD-011:** Command validators SHOULD be deterministic and SHOULD NOT mutate the workspace or run root. Scherzo MAY add detection for mutation in a future spec version, but version 1 does not require operating-system enforcement.

## 8. JSON Schema validator contract

JSON Schema validators provide declarative structural validation for admitted JSON.

**SOV-JS-001:** A JSON Schema validator MUST be declared with `type: json_schema` and MUST NOT map to artifact-specific Scherzo runtime code.

**SOV-JS-002:** Version 1 MUST support JSON Schema draft 2020-12. Scherzo MUST reject unsupported drafts as non-retryable configuration failures.

**SOV-JS-003:** Schema paths MUST be repository-relative, non-empty, and confined to the repository. Scherzo MUST reject absolute paths and traversal outside the repository before validation. Scherzo MUST also resolve the repository root and schema path to canonical paths at runtime and reject repository-relative symlinks whose target resolves outside the repository.

**SOV-JS-004:** A missing, unreadable, invalid-JSON, invalid-schema, symlink-escaped, or unsupported-draft schema file MUST be a non-retryable configuration failure.

**SOV-JS-005:** A payload that is valid JSON but does not satisfy the schema MUST be a retryable payload rejection when retry budget remains.

**SOV-JS-006:** JSON Schema diagnostics MUST identify the validator name, schema path, and a concise instance path when available. They MUST NOT include the full payload.

**SOV-JS-007:** The implementation MAY use an internal helper process for JSON Schema validation. That helper is Scherzo-owned infrastructure and is not the same mechanism as a workflow-declared command validator. Source-checkout runs MAY fall back to Scherzo's checked-in helper, but installed Scherzo MUST provide its own packaged helper and dependency runtime; workflow repositories MUST NOT be required to provide Scherzo's helper script for `type: json_schema` validation.

## 9. Validator failure and retry behavior

**SOV-FAIL-001:** Version 1 structured-output retry budget is controlled by `validation_retries` and MUST be `0` or `1`.

**SOV-FAIL-002:** A retry MUST be attempted only when the structured output is required, retry budget remains, and the failure is classified as retryable.

**SOV-FAIL-003:** Retryable structured-output failures include missing required output, blank required output, truncated capture, invalid JSON, wrong top-level JSON type, missing baseline required keys, Pi tool-call source failures caused by the agent response, JSON Schema payload rejection, and command validator exit status `1`.

**SOV-FAIL-004:** Non-retryable failures include invalid workflow declarations, unsupported validator declarations, missing or invalid JSON Schema files, unsupported schema drafts, command start failures, command timeouts, command exit status `2`, other command nonzero exits, validator infrastructure errors, and structured artifact write failures.

**SOV-FAIL-005:** If `required: false` and no structured output is present, Scherzo MUST record structured output as absent, MUST NOT run validators, MUST NOT persist a structured artifact, and MUST NOT spend retry budget. If an optional structured output is present but invalid, Scherzo MUST treat it as a validation failure but MUST NOT retry unless a future spec version says otherwise.

**SOV-FAIL-006:** Retry prompts MUST be concise. They SHOULD include the step id, artifact name, format, source instructions, top-level required keys, failure code, validator name and type when applicable, and a redacted diagnostic summary. They MUST NOT include the full invalid payload.

**SOV-FAIL-007:** Scherzo MAY classify specific agent failures as retryable under the structured-output retry budget when the agent failure is known to be transient and no payload was produced. Such classification MUST be explicit and MUST be covered by tests.

## 10. Artifact persistence semantics

**SOV-ART-001:** Structured artifacts MUST be persisted only after source extraction, baseline admission, and every configured validator pass.

**SOV-ART-002:** Invalid attempts MUST NOT be written as structured artifacts. They MAY be represented in step artifacts, retry diagnostics, and bounded redacted diagnostics.

**SOV-ART-003:** Persisted structured artifact payloads MUST be redacted JSON values.

**SOV-ART-004:** New structured artifacts SHOULD include validation metadata sufficient to understand why the payload was accepted: source type, optional source tool name, baseline schema type, baseline required keys, validator names, validator types, validator status, schema path and content hash for JSON Schema validators, command argv digest, timeout, working-directory mode, and command env keys.

**SOV-ART-005:** Structured artifact metadata MUST NOT persist command validator stdout or stderr on success and MUST NOT persist command env values in cleartext.

**SOV-ART-006:** If writing a structured artifact fails after validation passes, Scherzo MUST fail the step with a non-retryable structured-output artifact write error.

## 11. Diagnostics capture, truncation, and redaction

**SOV-DIAG-001:** Diagnostics that can reach step artifacts, retry prompts, retained artifacts, operator summaries, or logs MUST pass through Scherzo's standard redaction for known secret strings before they are retained or displayed.

**SOV-DIAG-002:** Diagnostics MUST be bounded. Scherzo MUST NOT retain unbounded validator stdout, validator stderr, payload snippets, parser dumps, or process transcripts.

**SOV-DIAG-003:** Command validator diagnostics MUST capture at most 8192 bytes of stdout and 8192 bytes of stderr per validator attempt in version 1. Scherzo MUST record whether stdout or stderr was truncated.

**SOV-DIAG-004:** Retry prompt diagnostic summaries MUST be capped at 1000 characters in version 1.

**SOV-DIAG-005:** Validator diagnostics SHOULD prefer stable codes, validator names, validator types, and concise paths over prose transcripts.

**SOV-DIAG-006:** Diagnostics MUST NOT include the full structured-output payload unless a later spec version defines an explicit opt-in debug mode with additional safety controls.

## 12. Environment variables exposed to command validators

Scherzo starts command validators from a clean environment. It may copy a small allowlist from the daemon environment, then adds workflow-declared `env`, then adds Scherzo-generated variables. Scherzo-generated variables always win.

**SOV-ENV-001:** The base command validator environment MAY copy only `PATH`, `LANG`, `LC_ALL`, and `TMPDIR` from the daemon environment when present. Scherzo MUST NOT inherit token or credential variables wholesale.

**SOV-ENV-002:** Workflow-declared command validator `env` keys MUST match `[A-Za-z_][A-Za-z0-9_]*`, values MUST be strings, and keys MUST NOT be `PATH`, `HOME`, `PWD`, or begin with `SCHERZO_`. Scherzo MUST reject invalid env declarations before running the validator.

**SOV-ENV-003:** Scherzo MUST expose these generated variables to command validators when the corresponding value is known, using an empty string for optional unavailable values:

| Variable | Meaning |
| --- | --- |
| `SCHERZO_CONFIG_DIR` | Directory containing the Scherzo orchestrator config. |
| `SCHERZO_REPO_ROOT` | Inferred repository root. |
| `SCHERZO_RUN_ROOT` | Per-run directory containing artifacts and run-local state. |
| `SCHERZO_WORKFLOW_ID` | Workflow id. |
| `SCHERZO_RUN_ID` | Run id. |
| `SCHERZO_STEP_ID` | Agent step id. |
| `SCHERZO_ATTEMPT_INDEX` | Numeric attempt index for the current agent attempt. |
| `SCHERZO_WORKSPACE_PATH` | Prepared workspace path for the agent step. |
| `SCHERZO_STRUCTURED_OUTPUT_ARTIFACT_NAME` | Structured artifact name. |
| `SCHERZO_STRUCTURED_OUTPUT_FORMAT` | Structured-output format, currently `json`. |
| `SCHERZO_STRUCTURED_OUTPUT_SOURCE_TYPE` | Source type, `final_response` or `pi_tool_call`. |
| `SCHERZO_STRUCTURED_OUTPUT_SOURCE_TOOL_NAME` | Tool name for Pi tool-call sources, otherwise empty. |
| `SCHERZO_VALIDATOR_NAME` | Validator name after defaults are assigned. |
| `SCHERZO_VALIDATOR_TYPE` | Validator type, currently `json_schema` or `command`. |
| `SCHERZO_VALIDATOR_INDEX` | Zero-based validator index in declaration order. |

**SOV-ENV-004:** Command validator env values are visible to the validator process and may appear in validator-authored output. Workflow authors SHOULD NOT place secrets in command validator `env` unless they intentionally trust the validator with those secrets.

## 13. Workflow fingerprinting requirements

**SOV-FP-001:** Workflow fingerprints MUST include all structured-output declaration fields that affect validation semantics, including `format`, `artifact_name`, `required`, `source`, baseline `schema`, `validators`, and `validation_retries`.

**SOV-FP-002:** Validator order is semantic. Fingerprints MUST preserve validator order and MUST change when validators are added, removed, reordered, renamed, or edited.

**SOV-FP-003:** JSON Schema validator fingerprint input MUST include validator name, type, repository-relative schema path, draft, and the SHA-256 hash of the schema file contents when available in the execution fingerprint context.

**SOV-FP-004:** Command validator fingerprint input MUST include validator name, type, argv list, timeout, working-directory mode, env keys, and SHA-256 digests of env values. It MUST NOT include command env values in cleartext.

**SOV-FP-005:** Fingerprints SHOULD include a structured-output validator contract version, currently `1`, so future incompatible changes to this contract can intentionally change workflow semantics.

**SOV-FP-006:** Version 1 does not require fingerprints to hash the contents of executable files referenced by command validator argv. This is intentionally deferred; workflow authors who need fingerprint changes for command implementation changes SHOULD change workflow-visible validator configuration or rely on run metadata outside the workflow fingerprint.

**SOV-FP-007:** During the compatibility window, legacy `validator: review_lane_draft` and the equivalent lowered command validator SHOULD canonicalize to the same fingerprint input when practical. If exact equivalence is not practical, Scherzo MUST document the one-time migration fingerprint change.

## 14. Backwards compatibility and migration rules

**SOV-COMPAT-001:** Scherzo MAY accept the legacy singular `validator` field during a migration window, but it MUST NOT represent it as a domain-specific runtime validator. It MUST lower supported legacy values to generic validators or reject them with a clear deprecation diagnostic.

**SOV-COMPAT-002:** If Scherzo supports legacy `validator: review_lane_draft`, it MUST lower that declaration to a generic command validator equivalent to invoking the review validation script through argv. Scherzo MUST NOT contain a `ReviewLaneDraftValidator` runtime constructor or equivalent runtime case.

**SOV-COMPAT-003:** New workflow YAML, examples, and docs MUST use the generic `validators` list. Legacy syntax MAY appear only in compatibility tests, migration notes, or retained historical artifacts.

**SOV-COMPAT-004:** Structured artifact readers MUST decode older retained structured artifacts that contain the old baseline schema shape and no generic validator metadata. Older artifacts SHOULD be interpreted as baseline admission metadata with an empty validator list when possible.

**SOV-COMPAT-005:** Removing legacy `validator` parsing is a separate migration decision. Before removal, the repository MUST inventory current usage and provide a clear diagnostic or migration path for remaining workflows.

## 15. Conformance expectations and test requirements

A conforming implementation must demonstrate behavior with automated tests and repository inventory checks.

**SOV-CONF-001:** Parser tests MUST cover valid declarations, defaults, invalid field types, invalid source declarations, invalid validator declarations, duplicate validator names, and rejection of declarations that contain both `validator` and `validators`.

**SOV-CONF-002:** Source extraction tests MUST cover final-response success, final-response missing/blank/truncated/invalid JSON, Pi tool-call success, missing tool call, wrong tool name, failed tool call, duplicate matching tool calls, sibling tool calls, malformed arguments, and non-object arguments.

**SOV-CONF-003:** Baseline and pipeline tests MUST prove that baseline admission runs before validators, validators run in declaration order, later validators do not run after an earlier failure, no validators means baseline-only acceptance, and structured artifacts are persisted only after validators pass.

**SOV-CONF-004:** JSON Schema tests MUST cover accepted payloads, schema payload rejection with instance-path diagnostics, missing schema files, invalid schema files, unsupported drafts, absolute paths, traversal paths, and diagnostics that do not echo full payloads.

**SOV-CONF-005:** Command validator tests MUST cover exit statuses `0`, `1`, `2`, other nonzero exits, start failure, timeout cleanup, redacted stdin, clean environment, reserved env rejection, Scherzo-generated env variables, stdout/stderr truncation, and concurrent stdout/stderr draining without deadlock.

**SOV-CONF-006:** Retry tests MUST cover retryable payload failures, non-retryable configuration failures, optional absent output, retry prompt content limits, and retry metadata.

**SOV-CONF-007:** Artifact tests MUST cover persisted redacted payloads, validation metadata, absence of invalid retained structured artifacts, artifact write failure classification, and backward-compatible decoding of older retained artifact JSON.

**SOV-CONF-008:** Fingerprint tests MUST prove fingerprints change when structured-output semantics change, including validator order, JSON Schema path, JSON Schema content hash when available, command argv, command timeout, working-directory mode, and env value digest changes.

**SOV-CONF-009:** Migration tests or repository checks MUST prove that `src/` contains no domain-specific runtime structured-output validator such as `ReviewLaneDraftValidator`, and that production workflow examples no longer use `validator: review_lane_draft` after migration.

## 16. Intentionally deferred behavior and open questions

The following behavior is intentionally deferred from version 1:

- non-JSON structured-output formats,
- multiple Pi tool calls combined into one structured artifact,
- `require_single: false` and `reject_sibling_tool_calls: false` semantics,
- more than one automatic structured-output retry,
- operating-system sandboxing or read-only enforcement for command validators,
- streaming validation,
- command executable content hashing in workflow fingerprints,
- unredacted payload delivery to workflow-declared command validators,
- success-time retention of command validator stdout or stderr, and
- legacy `validator` removal timing.

There are no open questions that block implementation of this specification. Deferred behavior MUST remain out of scope until a later spec revision changes the contract.
