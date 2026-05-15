# Design v1 workflow input and output contracts

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo operators should be able to tell, before a workflow starts, what named inputs it expects and, after it finishes, what named outputs it produced. Today a workflow is selected by a Linear label or scheduled job, receives issue context implicitly, and leaves behind step artifacts or external side effects. That is enough for a single workflow, but it is not enough to connect a research run to an ExecPlan drafting run, or an ExecPlan drafting run to an implementation run, without relying on comments, file naming conventions, or human memory.

After this plan is implemented, each workflow definition can declare a small v1 contract: named typed inputs, optional named execution-context values, and named typed outputs. The runtime records the actual input values it used and the actual output values it produced in retained run artifacts. Future workflow-to-workflow chaining can then say, explicitly, "take output `exec_plan` from run A and pass it to input `exec_plan` of run B". There is intentionally no `primary`, `main`, or default output in v1; every selection is by name.

## Problem Framing and Constraints

The operator problem is that Scherzo workflows currently communicate through implicit prompts, workspace files, Linear comments, and side effects. A person can often infer that a research workflow produced findings or that an implementation workflow produced a branch, but Scherzo cannot validate or record those facts in a stable shape. This makes chaining workflows risky because the next workflow cannot know which value to consume, whether it has the right shape, or where the value is retained.

The first version must be small. It must not introduce full workflow graph orchestration, a UI builder, a complex type hierarchy, or a global artifact registry. It must work with the current YAML workflow definitions, the current ledger and artifact retention model, and the existing distinction between issue-dispatched workflows and scheduled workflows. It must also allow non-file outputs, because an implementation workflow may produce a pull request URL, a branch, a merge commit, or a patch rather than a Markdown file.

The design in this plan treats contracts as declarative workflow metadata plus minimal runtime checks. Workflow definitions declare what they need and produce. Run start enforces required inputs only when Scherzo has enough information to resolve them. Run finish records outputs by name and fails a workflow only when a required declared output cannot be materialized. Optional inputs and outputs may be absent, but their absence is recorded.

## Strategy Overview

Add an optional top-level `contract` section to workflow YAML files and parse it into a new `scherzo/workflow_contract.gleam` module. A contract contains three named maps: `inputs`, `context`, and `outputs`. `inputs` are values consumed by prompts or workflow logic. `context` is for values that affect execution mechanics, such as `base_ref`; these are still named and typed but are recorded separately so future operators can distinguish "what the agent should reason about" from "where the workflow should run". `outputs` are values Scherzo should record after execution.

The v1 type vocabulary is deliberately small: `text`, `artifact[]`, `document.markdown`, `exec_plan`, `git_ref`, `url`, and `code_change`. These are string tags, not a class hierarchy. Compatibility validation is exact-name and exact-type by default, with one small convenience rule: a single produced artifact-like output may be appended as one element to an `artifact[]` input only when the future mapping says so explicitly. No automatic Markdown-to-text conversion, no subtype lattice, and no default output selection are introduced.

Runtime recording is additive. At run start, Scherzo writes a retained `inputs.v1.json` artifact for the run and appends a ledger record pointing to it. At run finish, Scherzo writes an `outputs.v1.json` artifact and appends a ledger record pointing to it. The existing step artifacts remain unchanged and continue to be written through `src/scherzo/state/artifact_store.gleam` and `src/scherzo/workflow_checkpoint.gleam`. Output records can point to retained Scherzo artifacts, URLs, git refs, or small structured JSON values.

## Alternatives Considered

The simplest possible alternative is documentation-only contracts in workflow YAML. That would help humans, but it would not solve the operator problem because Scherzo still could not reject a run missing a required input or tell a later run where to find an output. This plan therefore adds lightweight runtime recording and validation while keeping the schema small.

Another alternative is to model outputs through the existing `structured_output` field on agent steps only. That is too narrow because command steps can already produce useful stdout, research workflows may emit Markdown through commands, and implementation workflows may produce external values such as URLs or git refs. This plan reuses existing structured-output artifacts where they are present but does not require every output to be an agent structured output.

A third alternative is to build a full artifact registry with schemas, transforms, and graph execution. That would solve more future problems, but it is too large for v1. The current need is to name values, type them with a few coarse tags, retain references to actual values, and make future mapping validation possible.

## Risks and Countermeasures

The main risk is overbuilding the type system. The countermeasure is that v1 types are opaque strings from a fixed allowlist. Compatibility checks are exact except for explicit single-artifact-to-`artifact[]` wrapping. There are no primary outputs, no inheritance tree, no schema registry, and no automatic transforms.

Another risk is breaking existing workflows. The countermeasure is to make `contract` optional. Workflows without a contract parse and execute exactly as they do today, except that their workflow fingerprint code must include a stable representation of `contract: None` so old and new fingerprints remain deterministic.

A third risk is recording absolute local paths or workspace-specific paths that cannot be used after cleanup. The countermeasure is that v1 artifact references must use retained Scherzo artifact refs, repository-relative paths, URLs, git refs, or inline structured JSON. Absolute local paths are invalid in contract manifests. If a produced output comes from a workspace file, the runtime must copy or serialize it into a retained run artifact before recording it.

A fourth risk is making run-start enforcement too strict for current issue-dispatched workflows, which receive issue context implicitly rather than through an explicit input map. The countermeasure is to define built-in resolvers for the current implicit sources: `prompt` can be resolved from the tracker issue context for issue-dispatched runs, scheduled jobs can provide scheduled-run metadata through `scheduled_context`, and missing optional inputs are recorded as absent. Required inputs with no resolver fail before the first step starts.

The most likely false assumption is that all required outputs can be inferred from existing step artifacts. The plan tests this early by adding contract parser and output-source validation before wiring runtime materialization. If a workflow declares a required output but its `source` cannot be resolved to an existing step and field, the workflow file is invalid at load time.

## Progress

- [x] (2026-05-14 00:00Z) Drafted this ExecPlan from Linear issue LIV-292 and current repository inspection.
- [x] (2026-05-14 01:00Z) Incorporated adversarial review findings by closing the source grammar, runtime invocation, terminal sequencing, recovery, validation, and concrete-step gaps.
- [x] (2026-05-14 02:10Z) Added the v1 contract parser and data model without changing runtime behavior for workflows that omit `contract`.
- [x] (2026-05-14 02:20Z) Added contract fingerprinting, load/reload validation coverage, and focused parser tests.
- [x] (2026-05-14 02:35Z) Recorded actual run inputs at workflow start and appended a ledger record that references the retained input manifest.
- [x] (2026-05-14 02:50Z) Materialized and recorded actual named outputs at workflow finish and appended a ledger record that references the retained output manifest.
- [x] (2026-05-14 03:00Z) Added compatibility-validation helpers for future explicit workflow-to-workflow mappings.
- [x] (2026-05-14 03:10Z) Updated `examples/workflows/research.yaml` and tests to demonstrate the v1 contract shape without adding graph orchestration.
- [x] (2026-05-14 03:20Z) Ran format, test, lint, and ExecPlan validation commands and recorded the final outcome.
- [x] (2026-05-14 04:10Z) Applied plan-completion feedback by carrying contract manifest refs through projection and recovery, making manifest recording idempotent, adding missing runtime/recovery/reload tests, and rerunning the unit suite.
- [x] (2026-05-14 04:35Z) Completed follow-up validation repair by enforcing manifest schema headers and exact content hashes during manifest reuse, recognizing recovered started attempts as post-side-effect input recovery, refreshing projection/source guardrail fixtures, and rerunning format, tests, and lint gates.

## Surprises & Discoveries

- Observation: Current workflow definitions already have an agent-step `structured_output` concept, but it is step-scoped and JSON-only; it is not a workflow-level output contract.
  Evidence: `src/scherzo/workflow_dag.gleam` defines `StructuredOutputSpec` on `AgentStep`, and `src/scherzo/state/artifact_store.gleam` writes structured output artifacts under a step attempt path.

- Observation: Retained run artifacts already use stable relative refs under `runs/<run-id>/...`, which is a good base for v1 contract manifests.
  Evidence: `src/scherzo/state/artifact_store.gleam` builds refs such as `runs/<run_id>/<step_id>/attempt-<n>.json` and `runs/<run_id>/<step_id>/attempt-<n>/structured/<artifact_name>.json`.

- Observation: The source-size guardrail is strict for `src/scherzo/workflow_run.gleam`, so runtime contract additions had to stay within the updated baseline rather than adding a new helper module or broad refactor.
  Evidence: `direnv exec . gleam test` failed while `workflow_run.gleam` exceeded its source baseline, then passed after reducing the workflow-run line count to stay below the checked-in baseline.

- Observation: Plan-completion verification exposed that treating manifest ledger records as projection no-ops was too weak for restart safety.
  Evidence: Recovery now carries `workflow_run_inputs_recorded` and `workflow_run_outputs_recorded` artifact refs into `ResumeState`, and `direnv exec . gleam test` passes with recovery/idempotence coverage.

- Observation: Full-suite validation caught bookkeeping gaps in the follow-up repair: projection snapshots now serialize empty contract-manifest maps, and the intentional projection/workflow-run growth exceeded the checked-in source guardrail baselines.
  Evidence: The first full `direnv exec . gleam test` run failed on `projection_snapshot_golden_fixture_decodes_and_reencodes_test` and `source_guardrail_matches_checked_in_baseline_test`; after updating the fixture and baselines and adding final-attempt plus started-attempt recovery coverage, the suite passed with 1411 tests.

## Decision Log

- Decision: V1 contracts are enforced at run start for required inputs and at run finish for required outputs, while optional inputs and outputs may be absent and are recorded as absent.
  Rationale: Purely descriptive contracts would not enable safe chaining, but enforcing only the values Scherzo can resolve avoids blocking current workflows unnecessarily.
  Date: 2026-05-14

- Decision: `base_ref` is modeled as execution context, not as a normal prompt input, though it uses the same name-and-type contract machinery and can be targeted by a future explicit mapping.
  Rationale: `base_ref` changes where implementation work starts from. It may also appear in prompts, but its primary effect is on workspace setup and driver behavior.
  Date: 2026-05-14

- Decision: V1 has no `primary`, `main`, or default output concept.
  Rationale: Named output selection is simpler, avoids ambiguity, and directly matches the Linear issue requirement.
  Date: 2026-05-14

- Decision: Runtime records inputs and outputs as retained run artifacts referenced from new ledger records instead of embedding full values in the ledger.
  Rationale: The existing ledger is append-only operational history, while artifacts already hold larger retained payloads with refs and hashes.
  Date: 2026-05-14

- Decision: V1 accepts a closed YAML grammar for every source kind and normalizes each source to an explicit `kind` object before fingerprinting.
  Rationale: Parser authors and future mapping authors should not invent ad hoc scalar and map shapes, and workflow fingerprints must not depend on YAML spelling quirks.
  Date: 2026-05-14

- Decision: Contract parsing stays independent from workflow DAG validation; DAG-aware source checks live in `src/scherzo/workflow_dag.gleam` rather than in `src/scherzo/workflow_contract.gleam`.
  Rationale: `WorkflowDag` must carry an optional contract, so importing `workflow_dag` from the pure contract module would create a circular or muddy module boundary.
  Date: 2026-05-14

- Decision: Fresh workflow execution threads a workflow fingerprint and optional supplied mapped values through an explicit run-invocation type; normal issue and scheduled entry points compute the fingerprint and pass an empty supplied-value map.
  Rationale: Input manifests need the same fingerprint used by run-start ledger records, and future explicit mappings need a small runtime entry point without adding graph orchestration in v1.
  Date: 2026-05-14

- Decision: Additive input and output ledger record kinds remain ledger schema version `2` for this repository.
  Rationale: The schema version already represents the JSON envelope and decoder family; append-only record kinds are covered by fixture and decoder updates. Rollback to a binary that cannot decode the new records requires a pre-deployment state snapshot or a forward-compatible decoder.
  Date: 2026-05-14

- Decision: Projection stores input/output manifest artifact refs for recovery while operator-facing UI state still avoids presenting a new contract surface in v1.
  Rationale: Recovery must know when manifest records already exist to avoid duplicate or overwritten evidence. Keeping the stored refs out of current UI rendering preserves the additive rollout and avoids a broad terminal/control surface change.
  Date: 2026-05-14

- Decision: The checkpoint ledger writer treats existing contract manifest files and ledger records as idempotent evidence for a run id.
  Rationale: Recovery can encounter a manifest file without its ledger record or a ledger record from an earlier recovery attempt. Reusing a decoded same-run manifest and suppressing duplicate recorded records prevents overwrites and duplicate contract evidence.
  Date: 2026-05-14

- Decision: Contract manifest reuse validates `schema_version`, `artifact_type`, and the exact content hash before accepting an existing manifest for runtime or recovery reuse.
  Rationale: The recovery/idempotence rule allows appending a missing ledger record for an existing manifest file only after verifying it is the expected v1 workflow-contract manifest shape and the same bytes the runtime would otherwise write, not merely any JSON object with matching run fields.
  Date: 2026-05-14

## Outcomes & Retrospective

Implemented v1 workflow contracts as an optional workflow YAML feature. Workflows can now declare typed named inputs, execution context, and outputs; contract content participates in workflow fingerprints; invalid contract references fail loading/reload validation; contracted runs retain `inputs.v1.json` and `outputs.v1.json` manifests and append ledger records that point at those artifacts. The research example now demonstrates a required issue prompt input and required Markdown findings output, while existing workflows without `contract` remain valid.

Validation passed with `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, `direnv exec . gleam run -m glinter`, `direnv exec . gleam run -m scherzo_lint`, and `scripts/scherzo-execplan validate docs/plans/LIV-292-workflow-contracts-v1.md`. The lint commands still report the repository's existing warning inventory and no errors.

Plan-completion feedback was applied after the initial implementation. Recovery now remembers existing input and output manifest ledger refs, writes a single recovery input manifest when steps already started before input recording, skips duplicate terminal output manifests, and reuses verified same-run manifest files instead of overwriting them. Manifest decoders now reject wrong `schema_version` or `artifact_type` headers before a manifest can be reused, and manifest reuse requires the existing file hash to match the manifest the runtime would otherwise write. Additional tests cover scheduled context, optional workspace driver context, missing required outputs, terminal precedence for step failure, final-response/structured/inline outputs, optional missing outputs, recovery duplicate prevention, started-attempt input recovery, final accepted-attempt provenance, invalid contract reload safety, manifest-header rejection, and mismatched existing-manifest rejection. The full follow-up validation `direnv exec . gleam test` passed with 1411 tests.

## Context and Orientation

This repository is a Gleam service. Workflow definitions are YAML files. The main example config, `examples/scherzo.yaml`, maps workflow labels to workflow files under `routing.workflows`, for example `research: workflows/research.yaml` and `implementation: workflows/implementation.yaml`. The workflow files themselves live under paths such as `examples/workflows/research.yaml` and `examples/workflows/implementation.yaml`.

Workflow YAML parsing is implemented in `src/scherzo/workflow_dag.gleam`. The current top-level workflow shape is `version`, `id`, optional `description`, optional `workspace_profile`, optional `workspace_capabilities`, `max_parallel_steps`, and `steps`. A step is either an agent step with `prompt` and optional `structured_output`, or a command step with `run` and optional `timeout_ms`. Dependencies are expressed by `depends_on`, and workspace selection is expressed by `workspace`.

The runtime bundle loader in `src/scherzo/runtime_bundle.gleam` reads the config, resolves `routing.workflows`, loads each workflow YAML file, resolves prompt files relative to the workflow file, and stores workflow dependencies so `src/scherzo/orchestrator/workflow_reloader.gleam` can reload when the config, workflow, or prompt files change. Workflow selection for issue-dispatched runs uses the issue labels and `routing.workflow_label_prefix`.

Workflow invocations are represented today by run records and in-memory run state rather than by an explicit input object. For issue-dispatched runs, `src/scherzo/orchestrator/daemon.gleam` selects a workflow for a Linear issue and appends a `workflow_run_started` ledger record. That record currently includes `run_id`, `workflow_id`, `workflow_fingerprint`, `issue_id`, `issue_identifier`, `issue_fingerprint`, `observed_updated_at_ms`, and `run_root`. Scheduled jobs use the scheduled-run record family in `src/scherzo/state/record.gleam` and still ultimately execute a workflow by id.

The workflow executor is `src/scherzo/workflow_run.gleam`. It prepares per-step workspaces through `src/scherzo/workspace_run.gleam`, schedules runnable steps through `src/scherzo/workflow_scheduler.gleam`, executes agent and command steps, and writes checkpoints through `src/scherzo/workflow_checkpoint.gleam`. A `StepContext` already carries run identity, workflow id, step id, workspace path, config directory, issue identity, scheduled-job fields, and extra environment.

Run artifacts are retained through `src/scherzo/state/artifact_store.gleam`. A normal step artifact ref has the form `runs/<run-id>/<step-id>/attempt-<n>.json`. A structured output artifact ref has the form `runs/<run-id>/<step-id>/attempt-<n>/structured/<artifact-name>.json`. Step artifact contents are represented by `src/scherzo/step_artifact.gleam`, which includes stdout, stderr, final assistant response, exit code, duration, and optional structured-output outcome. Agent assistant result extraction is represented in `src/scherzo/result_artifact.gleam`.

The persistent ledger record definitions live in `src/scherzo/state/record.gleam`. The ledger schema version is currently `2`. The schema fixture `test/fixtures/schema/ledger_records_v2.jsonl` contains representative records and must be extended when new ledger record kinds are added. Projection code in `src/scherzo/state/projection.gleam` must ignore or index the new records in a way that does not break existing operator views.

## Preconditions and Verified Facts

The repository uses Gleam and the standard validation commands are run from the repository root through direnv when possible. The project config in `gleam.toml` declares `gleam_stdlib`, `gleam_erlang`, `gleam_otp`, `gleam_json`, `gleam_http`, `gleam_httpc`, `simplifile`, `yay`, and `birl` as dependencies, with `gleeunit`, `glinter`, and `glance` as development dependencies. The production lint policy requires `direnv exec . gleam run -m glinter` and `direnv exec . gleam run -m scherzo_lint`.

The current working copy was clean before this plan file was created. The workflow examples inspected were `examples/workflows/research.yaml`, `examples/workflows/implementation.yaml`, and `examples/scherzo.yaml`. Existing plan filenames under `docs/plans/` did not include `docs/plans/LIV-292-workflow-contracts-v1.md` before this plan was written.

The implementation must not depend on absolute local paths. Any recorded artifact path or manifest value must be repository-relative, a retained Scherzo artifact ref, a URL, a git ref, or inline structured JSON. If validation encounters a value shaped like an absolute local path, it must reject it with a clear error.

## Scope Boundaries

In scope for implementation is the v1 contract data model, YAML parsing, workflow fingerprinting, load-time validation, run-start input recording, run-finish output recording, retained manifest artifacts, focused compatibility helpers, example workflow updates, and tests.

Out of scope is graph orchestration, automatic dispatch of a downstream workflow, a UI for drawing mappings, a global artifact registry, cross-run garbage collection policy, rich content transforms, subtype reasoning, and automatic inference of implementation outputs from arbitrary git status. The implementation workflow must emit or identify its `code_change` output through an explicit declared source.

Existing workflows without `contract` remain valid. Existing step artifacts and structured-output artifacts remain valid. Existing scheduler behavior, workspace preparation, agent prompting, command execution, and Linear routing stay unchanged except for validation and recording around workflow start and finish.

## Proposed V1 Contract Schema

Add an optional top-level `contract` field to workflow YAML files parsed by `src/scherzo/workflow_dag.gleam`. The field has `version: 1` and up to three maps: `inputs`, `context`, and `outputs`. Map keys are the public names used by operators and future mappings. Names use the same conservative identifier shape as workflow and step ids: lowercase letters, numbers, hyphen, and underscore. Do not permit slashes, spaces, dots, or empty names in contract names; dots are reserved for type names such as `document.markdown`. Names must be unique within each map. The same spelling may appear in different maps only because future mappings explicitly target either `input` or `context`; examples should avoid that unless there is a good operator reason.

`contract.version` must be the integer `1`. `inputs`, `context`, and `outputs` are optional maps; an omitted map means no entries of that kind. Each entry has required key `type`, optional key `description`, optional key `required`, and optional key `source` under the rules below. No entry may contain `primary`, `main`, `default`, or unknown keys. `required` defaults to `true` for inputs and outputs and to `false` for context. `source` is required for every required input, required context value, and required output. Optional entries may omit `source`; if they cannot be resolved at runtime, their manifest status is `absent`.

Input sources are closed to these accepted YAML forms:

    source: issue_context
    source: scheduled_context
    source: mapped_output
    source:
      type: literal
      value: "literal prompt or parameter text"

`issue_context` is valid only for inputs and resolves from the tracker issue for issue-dispatched runs. `scheduled_context` is valid only for inputs and resolves from scheduled-job metadata for scheduled runs. `mapped_output` is valid for inputs and means the value must be supplied explicitly through the runtime `ContractRunValues` described below; v1 parses and records that supplied value but does not start downstream runs automatically. The literal map must contain exactly `type: literal` and a string `value`; extra keys or a missing `value` are invalid.

Context sources are closed to these accepted YAML forms:

    source: workspace_driver_base
    source: mapped_output
    source:
      type: literal
      value: "feature/liv-292"

`workspace_driver_base` is valid only for context entries and resolves from the workspace driver context when available. `mapped_output` has the same supplied-value semantics as it does for inputs, except the value is looked up in the supplied context map. Literal context values use the same exact two-key map as literal input values.

Output sources are closed to these accepted YAML forms:

    source:
      step: collect_findings
      field: stdout

    source:
      step: draft_execplan
      field: final_response

    source:
      step: summarize_change
      structured_output: code_change

    source:
      step: summarize_change
      inline_json: code_change

    source:
      type: url
      value: "https://example.invalid/pr/123"

    source:
      type: git_ref
      value: "feature/liv-292"

A field output source must contain exactly `step` and `field`; the field value is only `stdout` or `final_response`. A structured-output source must contain exactly `step` and `structured_output`; the value names an existing structured-output artifact on that step. An inline-JSON source must contain exactly `step` and `inline_json`; the value names an existing structured-output artifact whose decoded JSON is copied into the output manifest rather than referenced as a separate retained artifact. A static URL source must contain exactly `type: url` and `value`, and the value must be an `http` or `https` URL. A static git-ref source must contain exactly `type: git_ref` and `value`, and the value must be non-empty and contain no control characters. Scalar output sources are invalid.

Invalid source examples that tests must reject include `source: url`, `source: {type: literal}`, `source: {step: collect_findings, field: stdout, structured_output: findings}`, `source: {step: collect_findings, field: stderr}`, `source: {type: url, value: "not a url"}`, and any entry containing `primary: true`. The error text should name the contract entry and the offending source shape so an operator can fix the YAML without reading code.

For fingerprinting, the parser normalizes every accepted source to a canonical object with a `kind` field before `src/scherzo/workflow_fingerprint.gleam` serializes it. For example, `source: issue_context` becomes `{ "kind": "issue_context" }`, a field source becomes `{ "kind": "field", "step": "collect_findings", "field": "stdout" }`, and a static URL source becomes `{ "kind": "url", "value": "https://example.invalid/pr/123" }`. Contract entries are sorted by name inside each map for fingerprinting so YAML map order does not affect the workflow fingerprint.

The research workflow contract should be expressible as:

    contract:
      version: 1
      inputs:
        prompt:
          type: text
          required: true
          source: issue_context
        attachments:
          type: artifact[]
          required: false
      outputs:
        findings:
          type: document.markdown
          required: true
          source:
            step: collect_findings
            field: stdout

The ExecPlan drafting workflow contract should be expressible as:

    contract:
      version: 1
      inputs:
        prompt:
          type: text
          required: true
          source: issue_context
        context:
          type: artifact[]
          required: false
      outputs:
        exec_plan:
          type: exec_plan
          required: true
          source:
            step: draft_execplan
            field: final_response

The implementation workflow contract should be expressible as:

    contract:
      version: 1
      inputs:
        exec_plan:
          type: exec_plan
          required: true
          source: mapped_output
      context:
        base_ref:
          type: git_ref
          required: false
          source: workspace_driver_base
      outputs:
        code_change:
          type: code_change
          required: true
          source:
            step: summarize_change
            structured_output: code_change

The implementation example deliberately uses `context.base_ref` rather than `inputs.base_ref`. If a future workflow wants the agent to reason about the base ref as prompt content, it may also declare an input named `base_ref_text` or similar, but the execution-changing value remains context.

## Proposed V1 Data Structures

Create `src/scherzo/workflow_contract.gleam`. Keep it independent from runtime execution as much as possible so parser tests and compatibility tests can exercise it directly.

Define these public types:

    pub type Contract {
      Contract(
        version: Int,
        inputs: List(InputSpec),
        context: List(ContextSpec),
        outputs: List(OutputSpec),
      )
    }

    pub type ContractType {
      Text
      ArtifactList
      DocumentMarkdown
      ExecPlan
      GitRef
      Url
      CodeChange
    }

    pub type InputSpec {
      InputSpec(
        name: String,
        type_: ContractType,
        required: Bool,
        description: Option(String),
        source: Option<InputSource>,
      )
    }

    pub type ContextSpec {
      ContextSpec(
        name: String,
        type_: ContractType,
        required: Bool,
        description: Option(String),
        source: Option<ContextSource>,
      )
    }

    pub type OutputSpec {
      OutputSpec(
        name: String,
        type_: ContractType,
        required: Bool,
        description: Option(String),
        source: Option<OutputSource>,
      )
    }

Use lists rather than dictionaries in the public type so parsing can preserve YAML order for user-facing diagnostics while validation still enforces unique names.

Define source variants as:

    pub type InputSource {
      IssueContext
      ScheduledContext
      LiteralInput(value: String)
      MappedOutputSource
    }

    pub type ContextSource {
      WorkspaceDriverBase
      LiteralContext(value: String)
      MappedOutputContext
    }

    pub type OutputSource {
      StepField(step_id: String, field: OutputField)
      StructuredOutput(step_id: String, artifact_name: String)
      StaticUrl(url: String)
      StaticGitRef(ref: String)
      InlineJson(step_id: String, artifact_name: String)
    }

    pub type OutputField {
      Stdout
      FinalResponse
    }

`InlineJson` is for a structured-output payload that is small and should be copied as the output value. `StructuredOutput` is for a structured-output artifact that should be referenced as a retained run artifact. `StaticUrl` and `StaticGitRef` are useful for tests and scheduled workflows but should not be the normal way an implementation workflow reports a dynamic pull request or branch.

Add these public helper functions:

    pub fn parse(root: yay.Node) -> Result(Option(Contract), ContractError)
    pub fn validate_static(contract: Contract) -> Result(Nil, ContractError)
    pub fn type_to_string(type_: ContractType) -> String
    pub fn type_from_string(raw: String) -> Result(ContractType, ContractError)
    pub fn input_source_to_canonical_json(source: InputSource) -> json.Json
    pub fn context_source_to_canonical_json(source: ContextSource) -> json.Json
    pub fn output_source_to_canonical_json(source: OutputSource) -> json.Json
    pub fn compatible(source: ContractType, target: ContractType, mode: MappingMode) -> Bool

`workflow_contract.gleam` must not import `src/scherzo/workflow_dag.gleam`. `validate_static` checks unique names, allowed type strings, required entries having a source, forbidden `primary` or unknown keys, and closed source grammar. DAG-aware validation belongs in `src/scherzo/workflow_dag.gleam` as a private helper that runs after steps are parsed and before the final `WorkflowDag` is returned. That helper checks output sources referencing existing steps, `StepField` fields being valid for the target step kind, and `StructuredOutput` or `InlineJson` sources referencing an existing agent step structured-output artifact name. Command steps may use `Stdout`; agent steps may use `FinalResponse`; either may use a retained structured output only if that step already supports it.

## Minimal Artifact Reference Model

A v1 artifact reference is a JSON object embedded in input and output manifests. It is not a global registry entry. It has enough information for Scherzo to display, validate, and pass the value to a later run.

The shape is:

    {
      "type": "document.markdown",
      "status": "present",
      "ref_kind": "run_artifact",
      "ref": "runs/run-123/outputs/findings.md",
      "sha256": "<sha256>",
      "bytes": 1234,
      "media_type": "text/markdown",
      "source": {
        "step_id": "collect_findings",
        "artifact_ref": "runs/run-123/collect_findings/attempt-1.json",
        "field": "stdout"
      }
    }

The allowed `ref_kind` values are:

`run_artifact` for a retained Scherzo artifact under the existing artifact store. Use this for Markdown documents, ExecPlans, patches, and structured-output files that Scherzo has copied into run retention.

`url` for an external URL, such as a pull request URL. The value must parse as `http` or `https` and must not be treated as a local file.

`git_ref` for a branch, tag, commit, or other git revision string. V1 only validates that it is non-empty and does not contain control characters. It does not contact a remote.

`inline_json` for a small structured object, such as a `code_change` object containing optional fields `pr_url`, `branch`, `merge_commit`, `patch_ref`, and `notes`. The object must fit within the existing artifact limits or a new small manifest limit.

`absent` is represented by `status: "absent"` and no `ref_kind`. Use it for optional inputs or outputs that were not supplied or not produced.

Do not record arbitrary absolute local paths. A workspace file output must be copied into a retained run artifact first. A repository-relative path may appear in metadata for display, but it is not sufficient as the durable reference unless the file is also retained or the value is a git ref that can recover it.

## Storage and Manifest Changes

Extend `src/scherzo/state/artifact_store.gleam` with functions that write contract manifests and output blobs under existing run refs:

    pub fn workflow_inputs_ref(run_id: String) -> String
    pub fn workflow_outputs_ref(run_id: String) -> String
    pub fn workflow_output_blob_ref(run_id: String, output_name: String, extension: String) -> String
    pub fn write_workflow_inputs_manifest(store: Store, manifest: WorkflowInputsManifest) -> Result(ArtifactRef, ArtifactError)
    pub fn write_workflow_outputs_manifest(store: Store, manifest: WorkflowOutputsManifest) -> Result(ArtifactRef, ArtifactError)
    pub fn write_workflow_output_blob(store: Store, run_id: String, output_name: String, extension: String, contents: String) -> Result(ArtifactRef, ArtifactError)

Use refs such as:

    runs/<run-id>/inputs.v1.json
    runs/<run-id>/outputs.v1.json
    runs/<run-id>/outputs/<output-name>.md
    runs/<run-id>/outputs/<output-name>.json

Define manifest types in a new `src/scherzo/workflow_contract_manifest.gleam` module. Keep JSON encoding and decoding there rather than mixing manifest JSON into `workflow_run.gleam`.

The input manifest contains schema version, run id, workflow id, workflow fingerprint, issue id when present, contract version, named actual inputs, named actual context values, validation status, and diagnostics. Do not copy full issue comments into this file unless the existing prompt rendering already does so elsewhere. For `issue_context`, record a source descriptor, issue identifier, issue fingerprint, and a short display excerpt.

The output manifest contains schema version, run id, workflow id, workflow fingerprint, contract version, named outputs, status for each declared output, reference data for present outputs, and diagnostics for missing required outputs.

Extend `src/scherzo/state/record.gleam` with two new record bodies:

    WorkflowRunInputsRecorded(
      run_id: String,
      workflow_id: String,
      issue_id: String,
      artifact_ref: String,
      artifact_sha256: String,
    )

    WorkflowRunOutputsRecorded(
      run_id: String,
      workflow_id: String,
      issue_id: String,
      artifact_ref: String,
      artifact_sha256: String,
      produced_count: Int,
      missing_required_count: Int,
    )

Keep the ledger `schema_version` at `2`. In this repository, adding append-only record kinds is compatible with schema version 2 when the encoder, decoder, representative fixture, and schema guardrail tests all know the new kinds. Extend JSON encoding, decoding, fixture coverage, and projection handling. Projection should retain the latest input and output manifest refs per workflow run if that is a small change; otherwise it must explicitly ignore them for existing UI views and include a test proving replay does not fail on the new records.

## Runtime Invocation Interface

The workflow runtime needs an explicit invocation object so input manifests can record the same workflow fingerprint used by the run-start ledger record and so future explicit mappings can supply values without adding graph orchestration. Change the public runtime context in `src/scherzo/workflow_run.gleam` from `FreshRun(run_id: String)` to a shape like this:

    pub type ContractRunValues {
      ContractRunValues(
        inputs: Dict(String, workflow_contract_manifest.ManifestValue),
        context: Dict(String, workflow_contract_manifest.ManifestValue),
      )
    }

    pub type RunInvocation {
      RunInvocation(
        run_id: String,
        workflow_fingerprint: String,
        supplied_contract_values: ContractRunValues,
      )
    }

    pub type RunContext {
      FreshRun(RunInvocation)
      RecoveredRun(RecoveredRunContext)
    }

The exact manifest value type can be named differently if it lives in `src/scherzo/workflow_contract_manifest.gleam`, but it must carry the declared type, status, and reference or inline value needed to write the input manifest. Add `empty_contract_run_values()` for normal runs. Keep `execute(...)` as the current simple issue-dispatched entry point by computing `workflow_attempt.workflow_fingerprint(dag, orchestrator)` and passing empty supplied values. Add `execute_with_contract_values(...)` with the same arguments plus `ContractRunValues`; tests and future mapping code use this entry point. Update `execute_scheduled(...)` to compute the same fingerprint helper and pass empty supplied values unless a caller-specific scheduled mapping is later added.

Extend `ResumeState` and `RecoveredRunContext` so recovered runs carry `workflow_fingerprint`, any already-recorded input and output manifest artifact refs, and the final accepted step artifact refs needed for output provenance. `execute_with_resume(...)` must not build a recovered context with an empty workflow fingerprint. If older recovery data lacks the fingerprint, recompute it from the current `dag` and `orchestrator`, record a recovery warning, and use that value consistently in any new manifest written during recovery.

## Runtime Recording Behavior

At run start, after the workflow is selected and the workflow fingerprint is known but before any step workspace is prepared, resolve required inputs and context. For issue-dispatched workflows, `IssueContext` resolves from the current tracker issue. For scheduled workflows, `ScheduledContext` resolves from scheduled job metadata. `WorkspaceDriverBase` resolves from the workspace driver context if available; if it is not available and the context entry is optional, record it as absent. `MappedOutputSource` and `MappedOutputContext` look up the exact name in `ContractRunValues.inputs` or `ContractRunValues.context`; if a required mapped value is missing, fail before the first step with `workflow_required_input_missing:<name>` or `workflow_required_context_missing:<name>`. If a supplied value has the wrong type, fail before the first step with `workflow_contract_type_mismatch:<name>`. Optional mapped values may be absent.

Write the input manifest and append `workflow_run_inputs_recorded`. If writing this manifest fails, fail the run before starting steps. This is safe because no workspace side effects have occurred yet. Input recording must happen for contracted fresh runs before the first call to `prepare_step` or `prepare_recovered_step`.

During step execution, keep writing step artifacts exactly as today. Do not change the shape of `StepArtifact` for v1 unless a small helper field is unavoidable. Add a narrow runtime provenance map such as `Dict(String, ArtifactWritten)` keyed by final step id, or a similarly small structure, that records the final accepted attempt index, retained step artifact ref, sha256, and byte count returned by `write_step_artifact`. Populate it only after `step_finished` succeeds, and carry it through recovery from ledger replay. Existing structured-output validation remains step-scoped.

Terminal handling must follow one sequence for both success and failure so contracts do not create duplicate or contradictory terminal records. The sequence is: determine the scheduler outcome, materialize declared outputs from the available final step artifacts, write `outputs.v1.json`, append `workflow_run_outputs_recorded`, choose the final workflow outcome and reason, append the existing `workflow_finished` terminal record once, then run cleanup. If cleanup fails after a completed terminal record, keep the repository's existing behavior of appending a second failed terminal record for cleanup failure; do not append a completed record when output materialization has already changed the result to failure.

Failure precedence is deterministic. A run-start input or context failure happens before steps and therefore skips output materialization. A step checkpoint failure or step artifact write failure remains `checkpoint_failed:<detail>` and skips contract output materialization because the source artifact set is not trustworthy. If the scheduler succeeds but a required output is absent or invalid, the final outcome is `failed_fatal` with reason `workflow_required_output_missing:<name>`; include all missing required names in the output manifest diagnostics even if the failure reason names only the first. If the scheduler already failed because a step failed, materialize whatever outputs can be observed, record missing required outputs as diagnostics, and keep the final reason `workflow_step_failed` so the original cause is not hidden. If writing the output manifest fails after steps have run, the final reason is `workflow_output_manifest_failed:<detail>` for an otherwise successful scheduler outcome, or the original step-failure reason with `; workflow_output_manifest_failed:<detail>` appended for a failed scheduler outcome.

For each declared output, read the source step artifact or structured output artifact from the final provenance map. If the output source is `field: stdout` or `field: final_response`, copy the field contents to a retained output blob and record a `run_artifact` reference. Use `.md` and media type `text/markdown` for `document.markdown` and `exec_plan`; use `.txt` and media type `text/plain` for `text`; use `.json` for `code_change` or `inline_json`. If the source is a URL or git ref, validate and record the scalar reference. Absolute local paths are never durable references; a workspace file output must be copied into a retained run artifact before it is recorded.

## Recovery and Idempotence Rules

Input and output manifest recording must be idempotent for a run id. Recovery state should include `contract_inputs_recorded: Option(ArtifactWritten)` and `contract_outputs_recorded: Option(ArtifactWritten)`, where `ArtifactWritten` contains at least `ref`, `sha256`, and `bytes`. If recovery sees an existing input-recorded ledger entry, it must not rewrite `runs/<run-id>/inputs.v1.json` or append a duplicate `workflow_run_inputs_recorded` record. If no input record exists and no step has started, write the input manifest normally before resuming. If no input record exists but step artifacts already exist, write a recovery input manifest with validation status `recovered_after_steps_started`, include a diagnostic explaining that the original run predates input recording, and append exactly one input-recorded ledger record.

For outputs, if recovery sees an existing output-recorded ledger entry for a terminal run, it must not rewrite `runs/<run-id>/outputs.v1.json` or append a duplicate `workflow_run_outputs_recorded` record. If an in-progress recovered run later reaches a terminal scheduler outcome and no output record exists, materialize outputs once using the final accepted step artifact refs from recovery and current execution. If an output manifest file already exists at the expected ref but the ledger record is missing, read and hash the existing file; append one ledger record pointing to it only if it decodes as a valid v1 manifest for the same run and workflow. Otherwise write a new manifest with a diagnostic and fail safely rather than silently overwriting evidence.

The final accepted step artifact for an output source is the artifact for the successful or tolerated attempt that the scheduler marked finished for that step. Superseded attempts must never be used for output provenance. Recovery must rebuild the final-step provenance map from ledger `step_finished` records rather than scanning directories, because the ledger is the source of truth for which attempt was accepted.

## Future Explicit Mapping Model

Do not implement graph orchestration in this plan. Implement only the pure data structures and validation helpers that make a future mapping possible.

A future mapping should be able to say:

    source:
      run_id: research-run-1
      output: findings
    target:
      workflow: execplan
      input: context
      append: true

and:

    source:
      run_id: execplan-run-1
      output: exec_plan
    target:
      workflow: implementation
      input: exec_plan

and:

    source:
      run_id: prepare-base-run-1
      output: base_ref
    target:
      workflow: implementation
      context: base_ref

The compatibility helper reads the source output type from the source workflow contract or output manifest and the target input or context type from the target workflow contract. It rejects unknown names, absent source outputs, missing target inputs, required target values with no source, and incompatible types. It accepts exact type matches. It accepts appending one artifact-like output to `artifact[]` only when `append: true` is explicit. It rejects ambiguous mappings and never falls back to a primary output.

## Milestones

Milestone 1 adds the contract data model and parser. At the end, workflow YAML files may contain `contract`, parser tests cover valid and invalid shapes, and no runtime behavior has changed. This comes first because it proves the schema and diagnostics before touching execution.

Milestone 2 integrates contracts into workflow loading and fingerprinting. At the end, invalid contracts are rejected during config or workflow reload, contract changes affect workflow fingerprints, and existing workflows without contracts still load. This proves compatibility with current config loading and reload behavior.

Milestone 3 records actual inputs and context at run start. At the end, each contracted run has a retained `inputs.v1.json` artifact and a ledger record pointing to it. Required unresolved inputs fail before steps start. This is the first runtime change and is safe because it happens before side effects.

Milestone 4 records produced outputs at run finish. At the end, each contracted run has a retained `outputs.v1.json` artifact, required outputs are enforced, and optional outputs can be absent. This proves the output model against existing step artifacts.

Milestone 5 adds compatibility validation helpers and example contracts. At the end, tests can validate explicit future mappings from research findings to drafting context, ExecPlan output to implementation input, and base ref output to implementation context. Example YAML files demonstrate the intended shapes without adding graph orchestration.

## Plan of Work

In `src/scherzo/workflow_contract.gleam`, add the contract types, closed source grammar parser, static validators, type string helpers, canonical source JSON helpers, and compatibility helper. Keep this module pure and independent from runtime execution and from `src/scherzo/workflow_dag.gleam`. If identifier validation is private in `workflow_dag.gleam`, duplicate the small conservative name check in the new module rather than introducing a broad shared utility.

In `src/scherzo/workflow_dag.gleam`, add `contract: Option(workflow_contract.Contract)` to `WorkflowDag`, parse the optional top-level `contract` node after steps are read, and run a private DAG-aware validation helper after existing step validation. Keep `contract: None` for existing workflows. The private helper checks only facts that require the parsed workflow steps: source step existence, step-kind-specific fields, and structured-output artifact names.

In `src/scherzo/workflow_fingerprint.gleam`, include the contract in `dag_to_json_with_schema_root`. Use the canonical source JSON helpers and sort contract entries by name so fingerprints are stable regardless of YAML map order. Add a regression test that `source: issue_context` produces canonical JSON with `kind: issue_context` and fingerprints differently from `source: scheduled_context`.

In `src/scherzo/workflow_contract_manifest.gleam`, define input and output manifest types, the `ManifestValue` or equivalent value/reference type used by `ContractRunValues`, `ArtifactWritten` for refs and hashes, JSON encoders, decoders for tests, and reference validation helpers. Keep this module free of process or filesystem work.

In `src/scherzo/state/artifact_store.gleam`, add refs and write functions for input manifests, output manifests, and output blobs. Use the existing atomic write path and hash calculation. If the current artifact-store return type lacks byte count or sha256, extend it narrowly or add a helper that returns the `ArtifactWritten` data required by manifests and ledger records.

In `src/scherzo/workflow_checkpoint.gleam`, extend `Writer` with `write_workflow_inputs_manifest`, `workflow_inputs_recorded`, `write_workflow_outputs_manifest`, and `workflow_outputs_recorded`, or add a small adjacent contract checkpoint writer if extending `Writer` would make tests too noisy. Prefer extending `Writer` if the existing checkpoint pattern remains readable. The checkpoint layer, not `workflow_run.gleam`, should know how to append the new ledger record bodies.

In `src/scherzo/state/record.gleam`, add the two new ledger record variants, JSON names `workflow_run_inputs_recorded` and `workflow_run_outputs_recorded`, encoders, decoders, and tests. Update `test/fixtures/schema/ledger_records_v2.jsonl` and schema guardrail tests while keeping schema version `2`.

In `src/scherzo/state/projection.gleam` and the recovery code that builds `workflow_run.ResumeState`, make replay tolerate the new records and carry the latest input and output manifest refs plus final accepted step artifact refs into recovered runs. If retaining manifest refs in operator-facing projection state requires a broad UI change, ignore them explicitly for UI while still preserving them in recovery state and testing that projection does not fail.

In `src/scherzo/workflow_run.gleam`, add the `RunInvocation` and `ContractRunValues` interface, run-start input resolution before the first step batch starts, the final-step provenance map, and a terminal helper that materializes outputs before appending the workflow terminal record. Keep resolvers small and deterministic. Do not reread arbitrary files or scan artifact directories to discover outputs; use step artifacts and structured-output refs that the runtime accepted.

In `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/workflow_run.gleam`, and scheduled-run call sites, ensure the same fingerprint helper is used for the run-start ledger record and the input/output manifests. Normal issue and scheduled runs pass empty supplied contract values. Only tests or future explicit mapping code call `execute_with_contract_values`.

In `examples/workflows/research.yaml`, add a contract matching the research example only after parser and runtime recording tests pass. In the implementation example, add a contract only if the workflow has or gains an explicit source step for `code_change`; otherwise add the schema example in a test fixture workflow instead of pretending the current workflow records a code change it cannot actually produce. Do not change production workflow behavior merely for documentation.

## Concrete Steps

From the repository root, inspect the current tree before editing:

    $SCHERZO_WORKSPACE_DRIVER status --human

Expect a clean or intentionally scoped working copy. If unrelated changes exist, do not overwrite them.

Add `test/workflow_contract_test.gleam` with tests for `type_from_string`. Cover `text`, `artifact[]`, `document.markdown`, `exec_plan`, `git_ref`, `url`, and `code_change`, plus rejection of `markdown_document`. Run `direnv exec . gleam test test/workflow_contract_test.gleam` and expect it to fail because `src/scherzo/workflow_contract.gleam` does not exist yet.

Create `src/scherzo/workflow_contract.gleam` with `ContractType`, `type_from_string`, and `type_to_string`. Run the same targeted test and expect it to pass.

Add contract-name validation tests in `test/workflow_contract_test.gleam`. Cover `prompt`, `base_ref`, and `collect-findings` as valid names; cover empty string, `with space`, `document.markdown`, and `outputs/findings` as invalid. Implement the name validator in `workflow_contract.gleam` and rerun the targeted test.

Add source parser tests for input sources in `test/workflow_contract_test.gleam`. Cover scalar `issue_context`, scalar `scheduled_context`, scalar `mapped_output`, literal map with `type: literal` and `value`, and invalid literal map missing `value`. Implement only the input-source parser and rerun the targeted test.

Add source parser tests for context sources. Cover scalar `workspace_driver_base`, scalar `mapped_output`, literal map, and rejection of `issue_context` as a context source. Implement only the context-source parser and rerun the targeted test.

Add source parser tests for output sources. Cover `step` plus `field: stdout`, `step` plus `field: final_response`, `step` plus `structured_output`, `step` plus `inline_json`, static `type: url`, static `type: git_ref`, and invalid maps with extra keys or `field: stderr`. Implement only the output-source parser and rerun the targeted test.

Add whole-contract parser tests. Cover a valid research contract, omitted maps, default `required` values, rejection of unknown keys, rejection of `primary: true`, rejection of duplicate names if the YAML parser exposes duplicate keys, and rejection of a required output with no source. Implement `Contract`, `InputSpec`, `ContextSpec`, `OutputSpec`, `parse`, and `validate_static`, then rerun `direnv exec . gleam test test/workflow_contract_test.gleam`. Commit point: pure contract parsing and static validation pass without runtime changes.

Modify `src/scherzo/workflow_dag.gleam` so `WorkflowDag` carries `contract: Option(workflow_contract.Contract)`. Add the private DAG-aware validation helper in the same module. Add tests to `test/workflow_dag_test.gleam` for a workflow with no contract, a workflow with a valid contract, an output source referencing an unknown step, a command step declaring `final_response`, and a `structured_output` source that names an artifact missing from the agent step. Run `direnv exec . gleam test test/workflow_contract_test.gleam test/workflow_dag_test.gleam` and expect it to pass. Commit point: workflow loading accepts valid contracts and rejects invalid DAG references.

Modify `src/scherzo/runtime_bundle.gleam` or adjacent loader tests so invalid contract YAML fails bundle loading with a useful diagnostic. Add a reload-safety test in `test/workflow_reloader_test.gleam` or the existing runtime-bundle/reloader test file: start with a good workflow bundle, replace a dependency with a contract whose output references an unknown step, trigger reload, and assert the last known good bundle remains active. Run the targeted runtime-bundle or reloader test and expect it to pass.

Modify `src/scherzo/workflow_fingerprint.gleam` and `test/workflow_fingerprint_test.gleam`. Add tests that changing `outputs.exec_plan.type` changes the fingerprint, reordering contract maps does not change the fingerprint, and changing `source: issue_context` to `source: scheduled_context` changes the fingerprint. Run the fingerprint tests and expect them to pass. Commit point: contract changes participate in deterministic workflow fingerprints.

Create `src/scherzo/workflow_contract_manifest.gleam` and `test/workflow_contract_manifest_test.gleam`. Add manifest value and reference tests for present `run_artifact`, present `url`, present `git_ref`, present `inline_json`, absent optional value, and rejection of a placeholder value shaped like `<absolute-local-path>/findings.md` as a durable reference. Implement JSON encoders, decoders, `ManifestValue`, and `ArtifactWritten` or equivalent types. Run the manifest tests and expect them to pass.

Modify `src/scherzo/state/artifact_store.gleam` and `test/artifact_store_test.gleam` to write and read input manifests, output manifests, and output blobs under `runs/<run-id>/...`. Assert that writes return the retained ref, sha256, and byte count needed by ledger records. Run artifact-store tests and expect them to pass.

Modify `src/scherzo/state/record.gleam`, `test/state_record_test.gleam`, `test/fixtures/schema/ledger_records_v2.jsonl`, and any schema guardrail tests to include `workflow_run_inputs_recorded` and `workflow_run_outputs_recorded` while keeping schema version `2`. Run state record and schema guardrail tests and expect them to pass. Commit point: ledger encoding and decoding know the new record kinds.

Modify `src/scherzo/state/projection.gleam`, state recovery code, and tests so replay tolerates the new records, remembers manifest refs for recovery, and rebuilds final accepted step artifact provenance from `step_finished` records. Add tests for replaying old fixture records with no contract records and replaying new records with manifest refs. Run state recovery/projection tests and expect them to pass.

Modify `src/scherzo/workflow_checkpoint.gleam` and the tests that construct checkpoint writers so they can write the new manifest artifacts and append the new ledger records. Run workflow checkpoint and state recovery tests and expect them to pass. Commit point: the checkpoint layer records contract manifests without exposing ledger serialization to the workflow runner.

Modify `src/scherzo/workflow_run.gleam` to add `RunInvocation`, `ContractRunValues`, `empty_contract_run_values`, and `execute_with_contract_values`. Update `execute`, `execute_scheduled`, and `execute_with_resume` to populate workflow fingerprints as described above. Add compile-focused tests or update existing test helpers that construct `FreshRun`. Run the targeted workflow-run tests and expect them to compile and pass.

Modify `src/scherzo/workflow_run.gleam` to resolve and record inputs at run start. Add tests in `test/workflow_run_test.gleam` that a required `prompt` input from `issue_context` is recorded before any step-finished record, an optional `attachments` input is absent, a scheduled workflow with required `scheduled_context` records schedule metadata, optional `workspace_driver_base` context is absent when unavailable, a required `mapped_output` with no supplied value fails before any step starts, and `execute_with_contract_values` supplies an `exec_plan` mapped value successfully. Run targeted workflow-run tests and expect them to pass. Commit point: contracted run-start behavior is enforced before side effects.

Modify `src/scherzo/workflow_run.gleam` to maintain final-step provenance while step artifacts are written. Add a focused test that a retried step uses the accepted final attempt ref in output provenance and not a superseded attempt. Run targeted workflow-run tests and expect them to pass.

Modify `src/scherzo/workflow_run.gleam` to materialize and record outputs at run finish using the terminal sequence above. Add tests that `field: stdout` creates a Markdown output blob, `field: final_response` creates an ExecPlan output blob, `structured_output` creates or references a JSON output, `inline_json` copies a small JSON object into the manifest, optional missing output is absent, required missing output after scheduler success changes the final reason to `workflow_required_output_missing:<name>`, and step failure remains `workflow_step_failed` while output diagnostics are still recorded. Run targeted workflow-run tests and expect them to pass. Commit point: contracted run-finish behavior is enforced and terminal records are ordered.

Add recovery/idempotence tests. Cover recovery with an existing input manifest record, recovery without an input manifest before any step starts, recovery with step artifacts but no input manifest, recovery of a terminal run that already has an output manifest, and recovery where the manifest file exists but the ledger record is missing. Assert no duplicate input or output ledger records are appended. Run state recovery and workflow-run tests and expect them to pass.

Add compatibility helper tests in `test/workflow_contract_test.gleam` or `test/workflow_contract_mapping_test.gleam`. Cover exact `exec_plan` to `exec_plan`, exact `git_ref` to context `git_ref`, `document.markdown` to `artifact[]` with explicit append, and rejection of ambiguous, absent, incompatible, or missing names. Run compatibility tests and expect them to pass.

Update example workflow YAML or test fixture workflows to demonstrate the research, ExecPlan drafting, and implementation contracts. Run the full validation commands listed below. Final commit point: all tests, format checks, linters, and ExecPlan validation pass.

## Testing and Falsifiability

Parser tests in `test/workflow_contract_test.gleam` must prove the schema is small and strict. Write tests that parse `prompt: text` and `findings: document.markdown` successfully, reject unknown type `markdown_document`, reject duplicate `outputs.findings` if duplicate keys are observable, reject `primary: true` or any `primary` key with an error explaining that v1 selects outputs by name, and reject a required output with no source. Source grammar tests must cover every accepted input, context, and output source form listed in this plan and reject scalar output sources, missing literal values, maps with extra keys, `field: stderr`, non-HTTP URLs, and control-character git refs.

Workflow DAG tests in `test/workflow_dag_test.gleam` must prove contracts are validated against steps without creating a module cycle. A workflow with `outputs.findings.source.step: collect_findings` passes only when that step exists. A workflow with `field: stdout` on a command step passes. A workflow with `field: final_response` on a command step fails. A workflow with `structured_output: code_change` passes only when the named agent step declares a matching `structured_output.artifact_name`. A workflow with `inline_json: code_change` follows the same structured-output existence rule.

Runtime-bundle or workflow-reloader tests must prove reload safety. Start with a valid bundle, introduce a workflow contract with an unknown output source step, trigger reload, and assert that the reload reports an invalid dependency snapshot while the last known good bundle remains in effect. This is the test that falsifies the rollout claim if bad YAML can crash the daemon or replace a good bundle.

Fingerprint tests in `test/workflow_fingerprint_test.gleam` must prove that a contract change changes the fingerprint and YAML map reordering does not. The falsifying case is a test where two workflows differ only in `outputs.exec_plan.type` and produce the same fingerprint; that must fail before the fingerprint change and pass after it. A second falsifying case is a test where the same contract entries in different order produce different fingerprints; that must pass after canonical sorting.

Manifest tests in `test/workflow_contract_manifest_test.gleam` must prove the artifact reference model. A retained ref `runs/run-1/outputs/findings.md` is accepted. A URL `https://example.invalid/pr/1` is accepted as `ref_kind: url`. A git ref `feature/liv-292` is accepted as `ref_kind: git_ref`. A placeholder shaped like `<absolute-local-path>/findings.md` is rejected as a durable reference. An optional absent value encodes with `status: absent` and decodes back. An inline JSON `code_change` with no `pr_url`, `branch`, `merge_commit`, or `patch_ref` is rejected when the output is required.

Runtime input tests in `test/workflow_run_test.gleam` must prove run-start enforcement and scheduled-run support. Construct a small single-command workflow with required `prompt` from `issue_context`; execute it with a fake issue; assert that the checkpoint writer recorded an input manifest before any step-finished record. Construct a scheduled workflow with required `scheduled_context`; execute it through `execute_scheduled`; assert the input manifest includes the scheduled job id and does not require tracker issue text. Construct a workflow with optional `workspace_driver_base`; execute without a driver base and assert the context entry is absent. Construct another workflow with required `exec_plan` from `mapped_output`; execute without supplied mapped values; assert the workflow fails before command execution and no workspace step is prepared. Execute the same workflow through `execute_with_contract_values` with a supplied `exec_plan` value and assert the first step is allowed to run.

Runtime output tests in `test/workflow_run_test.gleam` must prove run-finish recording and terminal precedence. Construct a command workflow that prints `# Findings` to stdout and declares `outputs.findings` from that stdout; assert that the output manifest contains `findings`, type `document.markdown`, status `present`, and a `run_artifact` ref. Construct an agent-style workflow whose final response is the ExecPlan output and assert `.md` output retention. Construct a structured-output workflow and an inline-JSON workflow. Construct a workflow whose required output references a missing final artifact; if the scheduler otherwise succeeds, assert the final failure reason includes `workflow_required_output_missing:findings`. Construct a step-failure workflow with a missing required output and assert the final reason remains `workflow_step_failed` while the output manifest records the missing output diagnostic. Construct an optional missing output and assert the workflow can still succeed while the output manifest records `status: absent`.

Ledger, projection, and recovery tests in `test/state_record_test.gleam`, `test/state_recovery_test.gleam`, `test/workflow_run_test.gleam`, or adjacent files must prove new record kinds can be encoded, decoded, replayed, and ignored or projected without breaking old records. The fixture `test/fixtures/schema/ledger_records_v2.jsonl` must include one input-recorded and one output-recorded line under schema version `2`. Recovery tests must prove existing manifest records are not duplicated, missing pre-step input records are written once, recovered runs use final accepted step artifact refs, superseded attempt refs are ignored, and a terminal run with an existing output manifest does not rewrite it.

Compatibility tests must prove future chaining is explicit. A source output named `exec_plan` of type `exec_plan` maps to target input `exec_plan` of type `exec_plan`. A source output named `findings` of type `document.markdown` does not map to target input `prompt` of type `text` unless a future transform is explicitly introduced; v1 must reject it. A source output named `findings` can append to target input `context` of type `artifact[]` only when the mapping has explicit append semantics. A source output named `base_ref` maps to target context `base_ref` only when the target kind is context, not input, unless the workflow separately declares an input by that name.

The plan is falsified if existing workflows without `contract` stop loading, if invalid contract YAML replaces a last known good runtime bundle, if changing a contract does not change the workflow fingerprint, if a required missing input allows steps to run, if a scheduled workflow cannot record `scheduled_context`, if a supplied mapped value has no runtime path, if a required missing output is silently omitted, if output materialization creates duplicate terminal records outside the documented cleanup-failure case, or if any manifest records an absolute local path as a durable reference.

## Validation and Acceptance

From the repository root, run formatting and tests:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Expect formatting to report no changes needed and the test suite to pass. If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the same commands.

Run the production lint gates:

    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

Expect no lint errors. Warnings that already exist may remain, but this work must not add new production lint errors or use production `let assert`, `panic`, or `todo`.

Validate this ExecPlan source file:

    scripts/scherzo-execplan validate docs/plans/LIV-292-workflow-contracts-v1.md

Expect the command to accept the Markdown plan. Do not check in a generated HTML plan viewer.

Acceptance for the implementation is behavioral. A workflow without `contract` still runs. Invalid contract YAML is rejected during load or reload without replacing the last known good bundle. A workflow with a required `prompt` input records `runs/<run-id>/inputs.v1.json` before its first step and uses the same workflow fingerprint as the run-start ledger record. A scheduled workflow with `scheduled_context` records scheduled metadata without requiring issue text. A workflow with a `findings` output records `runs/<run-id>/outputs.v1.json` and a named `findings` ref after it finishes. A workflow with a missing required `exec_plan` mapped input fails before workspace side effects, while the same workflow can start when `execute_with_contract_values` supplies that value. A workflow with a missing required output after scheduler success fails after attempting to materialize outputs and leaves an output manifest explaining the missing value. A workflow that already failed because a step failed keeps that step-failure reason while still recording output diagnostics when possible. Recovery does not append duplicate contract ledger records. Compatibility helpers reject ambiguous chaining and require explicit output and input or context names.

## Rollout, Recovery, and Idempotence

Roll out additively. First release parser support with `contract` optional and no existing workflow files changed. Then add runtime recording for workflows that declare contracts. Existing runs and old ledger records remain valid because new records are append-only, old records keep their shape, and ledger schema version remains `2` with broader record-kind coverage.

If contract parsing breaks config reload, `src/scherzo/orchestrator/workflow_reloader.gleam` should keep the last known good runtime bundle, as it does for other invalid dependency snapshots. Bad workflow YAML must not crash the daemon; it should produce a clear invalid-reload diagnostic. The explicit reload-safety test in this plan is required before any example workflow contract is enabled.

If run-start input manifest writing fails, fail the run before steps start. This is safe to retry because no workflow workspace side effects should have happened. If recovery finds that steps already started before an input manifest was recorded, it writes one recovery manifest with `recovered_after_steps_started` and does not duplicate it on later resumes.

If run-finish output manifest writing fails, preserve the step artifacts that were already written and report the failure in the workflow outcome according to the precedence rules above. A later recovery or operator investigation can still inspect step artifacts. Recovery must use ledger-accepted step artifact refs, not directory scans, so a superseded attempt cannot accidentally become the output source.

Manifest writes are idempotent for a given run id. Retrying the same run recovery should produce the same refs and equivalent JSON when the same inputs and outputs are observed. If the manifest file exists and the ledger record is missing, the runtime may append a ledger record for that file only after decoding it and verifying the run id, workflow id, schema version, and hash. It must not silently overwrite a mismatched existing manifest.

Backing out the feature should be safe if no workflow requires contracts. Remove or ignore `contract` sections from workflow YAML files before rolling back to a binary that cannot parse them. New ledger records written by the feature are forward-only evidence; older binaries may not understand them, so rollback to an old binary requires a state snapshot from before deployment or an old binary patched to ignore the new record kinds.

## Artifacts and Notes

Important existing retained artifact refs are shaped as:

    runs/<run-id>/<step-id>/attempt-<n>.json
    runs/<run-id>/<step-id>/attempt-<n>/structured/<artifact-name>.json

The new contract artifacts should follow the same run-local pattern:

    runs/<run-id>/inputs.v1.json
    runs/<run-id>/outputs.v1.json
    runs/<run-id>/outputs/<output-name>.md
    runs/<run-id>/outputs/<output-name>.json

Example `code_change` output value for v1:

    {
      "kind": "code_change",
      "pr_url": "https://example.invalid/pull/123",
      "branch": "liv-292-contracts",
      "merge_commit": null,
      "patch_ref": null,
      "notes": "Implementation completed and tests passed."
    }

The `code_change` object permits several ways to describe the result without assuming all implementation workflows create pull requests. At least one of `pr_url`, `branch`, `merge_commit`, or `patch_ref` must be present for a required `code_change` output.

## Interfaces and Dependencies

No new package dependency is required for v1. Use existing `gleam/json`, `gleam/dynamic/decode`, `yay`, `simplifile`, and repository modules.

The final `WorkflowDag` type in `src/scherzo/workflow_dag.gleam` should include:

    pub type WorkflowDag {
      WorkflowDag(
        id: String,
        description: Option(String),
        workspace_profile: Option(String),
        workspace_capabilities: List(config_types.WorkspaceCapability),
        max_parallel_steps: Int,
        steps: List(WorkflowStep),
        contract: Option(workflow_contract.Contract),
      )
    }

The final manifest modules should expose JSON encoders and decoders so tests do not inspect raw strings. They should also expose the manifest value/reference type used for supplied mapped values and `ArtifactWritten` or an equivalent ref/hash/bytes record used by artifact-store and recovery code.

The final `RunContext` shape in `src/scherzo/workflow_run.gleam` should carry `FreshRun(RunInvocation)` rather than a bare run id, and `RunInvocation` should include `run_id`, `workflow_fingerprint`, and `supplied_contract_values`. The simple public `execute(...)` and `execute_scheduled(...)` functions remain for existing callers and should fill these fields automatically. `execute_with_contract_values(...)` is the explicit entry point for tests and later workflow-to-workflow mapping.

The final checkpoint interface should make it possible for `src/scherzo/workflow_run.gleam` to record manifests without knowing ledger serialization details. The final compatibility helper should be pure and should not read files, call Linear, inspect git, or dispatch a workflow.

## Deferred Work

Graph orchestration is deferred. This plan does not start a downstream workflow automatically after an upstream workflow finishes.

A UI or CLI for selecting outputs and mapping them into inputs is deferred. The compatibility helper exists so that a later UI or CLI has one place to validate mappings.

A rich type system is deferred. V1 type strings are intentionally coarse and fixed.

Automatic content transforms are deferred. In v1, `document.markdown` does not automatically become `text`, and `exec_plan` does not automatically become `document.markdown` unless a future explicit transform is designed.

A generalized artifact registry is deferred. V1 references retained run artifacts, URLs, git refs, or inline JSON values only.

Automatic inference of code-change outputs from workspace driver status is deferred. Implementation workflows must emit or declare the value they want recorded.

## Open Questions and Clarifications Needed

None.
