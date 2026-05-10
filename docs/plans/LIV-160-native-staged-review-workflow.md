# Rebuild staged review on native Scherzo structured outputs

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo operators need the staged review workflow to run as a normal Scherzo workflow, not as a Python script that launches an external agent backend. After this change, an operator can run a native Scherzo review workflow through the Scherzo workflow runner, see four ordinary `kind: agent` review lanes produce structured draft artifacts, see command steps independently verify or downgrade each lane's evidence claims, and receive a final review artifact that is safe to publish or inspect. The result is observable through retained JSON artifacts, raw-output artifacts for failed lanes, a preflight manifest, a dry-run publish manifest, and normal repository validation gates.

The most important behavior is not that agents write JSON. The important behavior is that Scherzo owns orchestration, step isolation, artifact retention, and failure metadata, while correctness claims are proven by deterministic command steps instead of being trusted because an agent wrote them. A failed, malformed, or no-finding lane must still leave enough structured evidence for an operator to understand what happened, and the final review must make that lane state visible.

This plan is not accepted until at least one native workflow run invokes `.scherzo/workflows/review-native.yml` through the Scherzo runner itself. `scripts/scherzo-review native-preflight` may wrap that runner for convenience, but it must not replace the direct native path with script-level lane execution.

## Problem Framing and Constraints

The current staged review prototype lives behind `scripts/scherzo-review`. It proved useful artifact concepts such as `ReviewBrief`, specialist lane results, synthesis, and local semantic fixtures. It also introduced a script-level backend choice for lanes, including an `external` backend. That backend is the wrong final architecture because it asks a helper script to run agents outside Scherzo's workflow engine, duplicate isolation and artifact-retention concerns, and decide whether a lane succeeded.

This plan solves the operator problem of reviewing code changes with reproducible staged evidence once Scherzo has native structured JSON/artifact output for agent steps. It does not implement that core structured-output platform. The plan assumes that platform work has already landed, but it treats that platform as a semantic dependency, not a syntax detail. Before helper expansion begins, implementation must prove that a `kind: agent` workflow step can request a JSON schema, persist raw output and transcript artifacts even when parsing fails, expose parsed-output paths or failure metadata to downstream command steps, continue after malformed output or agent-step failure, and run without remote mutation credentials.

This is review-workflow work only. The native workflow introduced here is a separate operator workflow under `.scherzo/workflows/review-native.yml`. Integration into the existing `.scherzo/workflows/implementation.yaml` staged-review path is deferred to LIV-115 after this workflow proves readiness. This issue must not post pull-request comments, update Linear, or mutate remote state during dry-run or preflight validation. It must keep LIV-115 blocked until the native workflow passes its semantic fixtures, direct-runner validation, and repository gates. It must not use `--agent-backend external` as the production review execution path, and it must not remove that rollback path in this issue.

## Strategy Overview

The right-sized approach is to reuse the artifact vocabulary already proved by the prototype while moving only the orchestration boundary into Scherzo. The first milestone is a platform-compatibility spike: create the smallest native structured-output workflow that proves exact YAML keys, the exact runner command, raw-output retention, failure metadata, artifact references, read-only lane containment, and downstream continuation after malformed and failed agent steps. The plan must be updated with those exact details before schema and helper work proceeds.

After the spike passes, the workflow prepares a `ReviewBrief` and shared review context with command steps, fans out four specialist review lanes as native Scherzo `kind: agent` steps, verifies their evidence requests with deterministic command steps, normalizes their structured drafts into trusted lane results, and synthesizes final results with the existing artifact helper logic. The native lanes are:

- `correctness`, focused on behavioral regressions and correctness blockers.
- `test-quality`, focused on missing, shallow, or misleading tests.
- `idioms-maintainability`, focused on repository conventions, readability, maintainability, and Gleam idioms.
- `security-performance`, focused on security and performance risks.

Each lane receives the same input bundle: the `ReviewBrief`, the unified diff, source metadata, changed-file context, validation status, and context snapshots for changed files. The lane emits a `ReviewLaneDraft` through native structured output. A draft may propose findings and evidence requests, but it is not allowed to mark correctness blockers as trusted. Each evidence request must link to one draft finding and state the exact claim, fixed evidence key, expected observation, and target fixture, test, or validation artifact that would prove or disprove that claim. Command steps then validate draft shape, run only allowlisted evidence checks owned by the harness, write an `EvidenceLedger`, and convert the draft plus ledger into a final `ReviewLaneResult` artifact. Synthesis consumes only `ReviewLaneResult` artifacts, so it remains independent of how the lane draft was produced.

The existing script remains useful as artifact machinery. It should keep preflight, schema validation, diff parsing, `ReviewBrief` generation, lane input bundle generation, evidence-ledger writing, lane-result normalization, synthesis, feedback-manifest writing, final review formatting, and dry-run publish-manifest writing. The script-level external backend should be bypassed for production and marked legacy, but actual removal is deferred to LIV-115 after the native operator path and implementation-workflow integration are proven.

## Alternatives Considered

The simplest alternative is to keep `scripts/scherzo-review run-lane --agent-backend external` and polish the command wrapper. That is insufficient because it leaves Scherzo unaware of the lane as a workflow agent step, hides structured-output failures inside a helper process, and continues duplicating artifact retention and mutation-safety checks outside the workflow engine.

Another option is to make each native lane emit a complete `ReviewLaneResult` directly. That is tempting but too trusting. A lane result contains `verified` and `blocking` semantics that should be assigned only after deterministic evidence checks. The plan instead introduces a draft artifact for agent claims and uses command steps to produce the final lane result.

A third option is to remove `scripts/scherzo-review` entirely and rebuild all artifact logic inside the workflow runtime. That is too large for this issue and would discard working preflight and synthesis logic. Keeping the script as a command-step helper gives a smaller, safer migration path.

## Risks and Countermeasures

The native structured-output platform may not yet support the semantic contract this workflow needs. If agent failure stops the workflow, if malformed raw output is not retained, if downstream command steps cannot read failure metadata, or if a stable run-root cannot be referenced, the native review graph cannot safely synthesize failed lane results. The first milestone therefore proves those behaviors in a minimal native workflow before any broad helper work. If the spike fails, stop implementation, record the failed contract in this plan's Surprises & Discoveries section, and keep LIV-160 blocked on the platform issue rather than building a script-level workaround.

A native agent could emit malformed JSON, omit required fields, cite paths outside the repository, or claim verified evidence it did not prove. The workflow catches this before synthesis: native structured-output support must retain raw output and failure metadata, `scripts/scherzo-review normalize-lane-result` must synthesize a failed `ReviewLaneResult` for malformed output, and `scripts/scherzo-review verify-evidence` must reject absolute paths, parent-directory escapes, and unallowlisted evidence commands.

Evidence verification could falsely trust an agent claim if it only records that an allowlisted command exited 0. The ledger must instead prove specific claims. Every `EvidenceRequest` must name a draft finding ID, a fixed evidence key, the claim it supports, the expected observation, and the target test, fixture reproduction, schema artifact, or static scan owned by the harness. A ledger verdict verifies only that one finding when the expected observation is met. Whole-repo green gates such as `gleam_test`, `glinter`, or `scherzo_lint` are context unless they are tied to a named reproduction, target test, or assertion for that finding.

A correctness lane could overstate a static suspicion as a blocking bug. The command normalizer must enforce the existing policy: correctness findings can remain blocking only when an evidence ledger has a `verified` verdict for the same draft finding and the evidence type is `test`, `runtime`, or `reproduction`. Otherwise the finding is downgraded to a non-blocking suspicion and the downgrade is recorded in `synthesis_actions`.

A lane could fail while other lanes succeed. Synthesis must still produce a final review artifact with `execution_summary.state` set to a lane-failure state, a lane status entry for the failed lane, and no fabricated findings from that lane. The malformed-output and lane-failure scenarios prove this behavior.

The workflow could accidentally mutate the working tree or remote state. Prompt text is not sufficient containment. Native review lanes must run with the strongest workflow-level read-only policy available, must not inherit GitHub or Linear write tokens, must not receive arbitrary shell tools, and must have no access to `SCHERZO_REVIEW_AGENT_COMMAND`. The workflow must include pre/post dirty-tree checks with `jj status --color=never`; validation fails if a lane changes tracked files. All preflight and dry-run command artifacts must record `remote_mutations: "none"` and must produce local artifacts only under `tmp/` or the Scherzo run artifact root.

The rollout could strand operators between two review paths. This issue keeps the new native workflow separate from `.scherzo/workflows/implementation.yaml` and keeps the existing command-lane path available as rollback. LIV-115 owns integration into the implementation workflow and any removal of the external backend.

## Progress

- [x] (2026-05-09 00:00Z) Drafted the ExecPlan for LIV-160 from the Linear ticket and a small inspection of the current review artifact helpers.
- [x] (2026-05-09 00:30Z) Incorporated adversarial review feedback: platform-contract spike, per-finding evidence semantics, read-only lane containment, rollout boundary, publish/feedback artifact contracts, and targeted tests.
- [x] (2026-05-09 18:10Z) Added `.scherzo/workflows/review-native-contract-spike.yml` using the current structured-output YAML keys: `structured_output.format: json`, `artifact_name`, `required`, `schema.type: object`, `schema.required`, and `on_failure: continue`. The current tree does not expose a direct single-workflow runner CLI or workflow-level read-only/no-write key, so this spike is checked in and parseable but was not executed as a live agent run.
- [x] (2026-05-09 18:25Z) Implemented schema additions for lane drafts, evidence ledgers, feedback manifests, and dry-run publish manifests in `docs/schemas/review-artifacts.v1.schema.json`, plus manual validation in `scripts/scherzo-review`.
- [x] (2026-05-09 19:05Z) Added native workflow command helpers (`prepare-native`, `verify-evidence`, `normalize-lane-result`, `apply-feedback`, `publish`, `native-preflight`) and the separate `.scherzo/workflows/review-native.yml` operator workflow definition.
- [x] (2026-05-09 19:25Z) Added targeted helper tests for draft path safety, evidence-request mapping, generic green-test context handling, correctness downgrade behavior, malformed draft containment, publish/feedback manifests, and native preflight provenance validation.
- [x] (2026-05-09 19:40Z) Added native preflight semantic fixture coverage for PR #80, inverted auth/control-condition, static suspicion, malformed output, lane failure, no-finding scenarios, and working-tree mutation containment through local native-draft fixtures with native lane provenance.
- [x] (2026-05-09 19:55Z) Validated the initial local native preflight manifest, legacy fixture preflight, formatting, Gleam tests, and lint gates. At that stopping point direct live runner validation was still blocked by the absence of a direct single-workflow runner command.
- [x] (2026-05-10 02:00Z) Added a direct local Scherzo workflow runner command, `direnv exec . gleam run -- workflow run <workflow.yml> --run-root <dir> --run-id <id> --native-review-scenario <id>`, which executes a single workflow DAG through `workflow_run.execute` with fixture native agent responses and retained structured-output artifacts.
- [x] (2026-05-10 02:05Z) Rewired `.scherzo/workflows/review-native.yml` downstream verification and normalization commands to consume the actual Scherzo artifact-store paths under `$SCHERZO_RUN_ROOT/.scherzo-state/artifacts/runs/$SCHERZO_RUN_ID/...` for structured-output payloads and step failure metadata.
- [x] (2026-05-10 02:06Z) Replaced `scripts/scherzo-review native-preflight` script-level native draft simulation with runner invocations of `.scherzo/workflows/review-native-contract-spike.yml` and `.scherzo/workflows/review-native.yml`; the generated manifest records the runner command, run summaries, native lane step provenance, structured-output paths, and contract-spike failure metadata.
- [x] (2026-05-10 02:08Z) Ran the native contract spike and all seven native preflight scenarios through the Scherzo workflow runner, then validated `tmp/scherzo-review-native-preflight/preflight-manifest.v1.json --require-cutover-ready` with `cutover_readiness.status` equal to `ready`.
- [x] (2026-05-10 02:30Z) Manually completed the blocked feedback cycle after the Scherzo run crashed, reran the direct native runner smoke, native preflight, legacy fixture preflight, local native workflow run, formatting, full Gleam test suite, glinter, and Scherzo lint gates; all required acceptance gates passed, including `gleam test` reporting `1036 passed, no failures` and both lint commands reporting `0 errors`.
- [x] (2026-05-10 02:55Z) Ran the manual staged review, fixed the blocking production-safety false positive by making the review helper detect actual `todo` constructs rather than legitimate issue-state values such as `issue_state.todo_state()`, reran staged review synthesis with `REVIEW_BLOCKING_FINDINGS=0`, and published the implementation PR for human acceptance.

## Surprises & Discoveries

- Observation: `scripts/scherzo-review` is a Python executable that already exposes `dry-run`, `run-lane`, `synthesize`, `preflight`, and `validate` subcommands.
  Evidence: The inspected parser in `scripts/scherzo-review` defines those subcommands and currently includes `--agent-backend` for `run-lane` and `preflight`.

- Observation: The existing lane IDs are already the four desired specialist lanes.
  Evidence: `scripts/scherzo-review` defines `SPECIALIST_LANES` entries for `correctness`, `test-quality`, `idioms-maintainability`, and `security-performance`.

- Observation: The existing harness already knows how to retain the inputs that native workflow lanes should receive.
  Evidence: `scripts/scherzo_review/agent_lane_harness.py` writes input artifacts named `review-brief.v1.json`, `diff.patch`, `source-metadata.v1.json`, `changed-files.v1.json`, `validation-status.v1.json`, and `context-manifest.v1.json`.

- Observation: The current artifact schema already defines `ReviewBrief`, `ReviewFinding`, `ReviewLaneResult`, `ReviewSynthesis`, `FinalReviewArtifact`, and `PreflightManifest`, but not a separate native lane draft artifact.
  Evidence: `docs/schemas/review-artifacts.v1.schema.json` lists those artifact definitions under its top-level `oneOf`.

- Observation: The structured-output platform supports `structured_output` on `kind: agent` steps, schema-required keys, structured output artifact metadata in step artifacts, and `on_failure: continue`, but command-step templates do not currently interpolate prior step artifact paths for normal issue workflows.
  Evidence: `src/scherzo/workflow_dag.gleam` parses the structured-output keys used in `.scherzo/workflows/review-native.yml`; `src/scherzo/step_artifact.gleam` exposes structured-output metadata as template locals for agent prompts; `src/scherzo/workflow_run.gleam` renders scheduled command templates but normal command steps run literal shell strings with only `SCHERZO_*` environment variables.

- Observation: The checked-in `dogfood-jj` workspace profile does not provide declared workspace capabilities such as `assert-only`, so adding `workspace_capabilities: [status, diff, changed-files, assert-only]` to the new workflow makes the runtime bundle fail to load.
  Evidence: An initial `gleam test` run failed with `workspace_capabilities_unavailable` for `review-native`; removing the top-level capability declaration restored the checked-in workflow bundle tests.

- Observation: The current CLI originally did not expose a direct single-workflow local runner command that could run `.scherzo/workflows/review-native.yml` without routing through Linear or a scheduled job, and plan-completion feedback correctly treated that as a blocking acceptance gap.
  Evidence: Before the repair, `src/scherzo/main.gleam` supported daemon mode, `--once`, Linear checks, doctor, control commands, and pi probe, but not a command such as `scherzo workflow run <path>`. The repair added `workflow run <workflow.yml> [--run-root <dir>] [--run-id <id>] [--native-review-scenario <id>]` and validated it with `direnv exec . gleam run -- workflow run .scherzo/workflows/review-native-contract-spike.yml --run-root tmp/scherzo-native-contract-smoke --run-id native-contract-smoke --native-review-scenario contract-spike`.

- Observation: The native structured-output artifact path available to downstream commands is the Scherzo artifact-store wrapper file, not the lane-local draft path used by the first implementation.
  Evidence: Runner summaries for `tmp/scherzo-review-native-preflight` record paths such as `tmp/scherzo-review-native-preflight/contract-spike/native-run/.scherzo-state/artifacts/runs/native-contract-spike/valid_lane/attempt-1/structured/valid_draft.json`; `scripts/scherzo-review` now unwraps the wrapper's `payload` before validating a `ReviewLaneDraft`.

- Observation: In local direct-runner mode, inherited `SCHERZO_REPO_ROOT` can point at the parent dogfood checkout instead of the dedicated repair workspace.
  Evidence: The first direct `review-native` smoke attempted to run an older `scripts/scherzo-review` without `prepare-native`; setting `SCHERZO_REPO_ROOT` to `path.absolute(".")` in the local runner's command-step environment made the runner use this workspace's helper script.

## Decision Log

- Decision: Express the four review lanes as native Scherzo `kind: agent` workflow steps instead of invoking a script-level external agent backend.
  Rationale: Scherzo should own agent execution, structured output retention, failure metadata, artifact dependency wiring, and lane isolation.
  Date: 2026-05-09

- Decision: Make the first implementation milestone a native structured-output compatibility spike.
  Rationale: The workflow depends on semantic platform behavior: raw-output retention, downstream continuation after failed or malformed agent steps, stable artifact references, read-only containment, and an operator runner command. Those must be proven before helper expansion.
  Date: 2026-05-09

- Decision: Add a `ReviewLaneDraft` artifact instead of letting agents emit final `ReviewLaneResult` artifacts directly.
  Rationale: Agent output is useful review analysis, but the workflow must not trust agent claims about verified evidence or blocking correctness bugs until command steps verify them.
  Date: 2026-05-09

- Decision: Make evidence verification a normal command step per lane and require each verdict to link to one draft finding.
  Rationale: Evidence checks must be deterministic, allowlisted, reproducible, and specific enough that a ledger proves a claim rather than merely showing that an unrelated command exited 0.
  Date: 2026-05-09

- Decision: Require workflow-level read-only and no-remote-mutation containment for native review lanes.
  Rationale: Prompt instructions and `remote_mutations: "none"` fields are audit signals, not access-control mechanisms. The workflow must prevent or detect file and remote mutations.
  Date: 2026-05-09

- Decision: Keep `scripts/scherzo-review` as the artifact helper, preflight, normalization, synthesis, validation, feedback-manifest, and dry-run publish entrypoint.
  Rationale: The prototype already contains valuable, local, no-side-effect artifact machinery. Reusing it avoids a risky rewrite of unrelated review logic.
  Date: 2026-05-09

- Decision: Treat `.scherzo/workflows/review-native.yml` as a separate operator workflow in this issue; defer integration into `.scherzo/workflows/implementation.yaml` to LIV-115.
  Rationale: A separate workflow lets this issue prove native lane semantics without changing the normal implementation workflow's rollback path.
  Date: 2026-05-09

- Decision: Deprecate production use of the script-level external backend in this issue, but defer actual removal to LIV-115.
  Rationale: Removing `run_external_agent`, `SCHERZO_REVIEW_AGENT_COMMAND`, or external-backend readiness logic before cutover would reduce rollback options while the native path is still being proven.
  Date: 2026-05-09

- Decision: Define publish and feedback manifests as schema-validated local artifacts.
  Rationale: Final validation should not rely on ad hoc JSON checks for artifacts that gate publication or feedback application; they need explicit `remote_mutations: "none"`, mode, input references, and output references.
  Date: 2026-05-09

- Decision: Keep LIV-115 blocked until native workflow validation passes.
  Rationale: LIV-115 should cut over only after the native workflow proves direct runner invocation, fixture parity, malformed-output handling, lane-failure handling, no-finding behavior, no-mutation containment, and normal repository gates.
  Date: 2026-05-09

- Decision: Do not route `review-native` from `.scherzo/scherzo.yaml` in this issue.
  Rationale: Adding the workflow to checked-in Linear routing changes the Linear contract and dogfood dispatch surface. The plan calls for a separate operator workflow artifact, while LIV-115 owns implementation-workflow integration and production routing.
  Date: 2026-05-09

- Decision: Use dirty-tree checks and provenance validation as the available containment mechanism rather than declaring unavailable workspace capabilities.
  Rationale: The current `dogfood-jj` profile does not provide `assert-only` or other workspace capability declarations, so a top-level `workspace_capabilities` requirement prevents the config bundle from loading. The workflow still captures `jj status --color=never` before and after native lanes, and the native preflight mutation fixture records containment.
  Date: 2026-05-09

- Decision: Keep `native-preflight` as a local semantic preflight wrapper until Scherzo exposes a direct single-workflow runner command.
  Rationale: The repository has native structured-output parsing and artifact storage, but no CLI for `scherzo workflow run .scherzo/workflows/review-native.yml`. The wrapper exercises the new draft, evidence, normalization, synthesis, feedback, publish, provenance, and fixture semantics without calling `run-lane`; live direct-runner validation remains a gate before LIV-115 cutover.
  Date: 2026-05-09

## Outcomes & Retrospective

Completed the native review artifact and helper migration while keeping the existing implementation workflow and external backend rollback path intact. Operators now have a checked-in `.scherzo/workflows/review-native.yml` graph, schema-backed `ReviewLaneDraft`, `EvidenceLedger`, `FeedbackManifest`, and `PublishManifest` contracts, deterministic evidence verification and lane normalization commands, dry-run feedback/publish artifacts, and native preflight scenarios that prove same-finding evidence linkage, correctness downgrade behavior, malformed-output containment, lane-failure containment, no-finding behavior, and mutation-containment recording.

The plan-completion blocking gap found during the first verification pass has been repaired. The tree now exposes a direct local runner command, `direnv exec . gleam run -- workflow run <workflow.yml> --run-root <dir> --run-id <id> --native-review-scenario <id>`, and `scripts/scherzo-review native-preflight` invokes `.scherzo/workflows/review-native-contract-spike.yml` plus `.scherzo/workflows/review-native.yml` through that runner instead of simulating native lane drafts in Python. Downstream evidence and normalization commands consume the actual Scherzo artifact-store structured-output paths and step metadata under `$SCHERZO_RUN_ROOT/.scherzo-state/artifacts/runs/$SCHERZO_RUN_ID/...`.

Final validation evidence was collected from this workspace on 2026-05-10: `python3 -m py_compile scripts/scherzo-review`; a direct contract spike runner smoke; native preflight for `pr-80`, `inverted-auth-control-condition`, `static-suspicion`, `malformed-agent-output`, `lane-failure`, `no-findings`, and `lane-mutates-worktree`; `scripts/scherzo-review validate --artifact tmp/scherzo-review-native-preflight/preflight-manifest.v1.json --require-cutover-ready`; a normal direct local run of `.scherzo/workflows/review-native.yml`; legacy fixture preflight and cutover validation; manual staged review synthesis with `REVIEW_BLOCKING_FINDINGS=0` and `REVIEW_REMOTE_MUTATIONS=none`; `direnv exec . gleam format --check src test`; `direnv exec . gleam test` with `1036 passed, no failures`; `direnv exec . gleam run -m glinter` with `0 errors`; `direnv exec . gleam run -m scherzo_lint` with `0 errors`; and `direnv exec . ./scripts/scherzo-implementation validate`/publish-time SelfCI with `FINAL_VALIDATION=passed`. LIV-115 remains responsible only for cutting the proven native path into the implementation workflow and for removing the legacy external backend after operator approval.

## Context and Orientation

Scherzo is a workflow system. In this plan, a workflow step with `kind: agent` means a Scherzo-managed step that prompts a model, captures its transcript, and now, as a prerequisite from separate platform work, persists a structured JSON artifact. A command step means a deterministic process run by the workflow, such as `scripts/scherzo-review validate`, `gleam test`, or a helper that normalizes JSON.

The existing implementation workflow is `.scherzo/workflows/implementation.yaml`. It currently generates a staged review brief with `scripts/scherzo-review dry-run`, then runs the four staged review lanes as `kind: command` steps that invoke `scripts/scherzo-review run-lane`. Those command-lane steps choose their backend with `SCHERZO_STAGED_REVIEW_AGENT_BACKEND`, defaulting to the heuristic backend. This plan does not change that production workflow. It adds a separate native operator workflow so LIV-160 can prove native agent-lane semantics before LIV-115 wires the path into implementation runs.

The review workflow's vocabulary is stored in `docs/schemas/review-artifacts.v1.schema.json`. The important existing artifact types are `review_brief`, `review_lane_result`, `review_synthesis`, `final_review`, and `preflight_manifest`. A `ReviewBrief` is an orientation artifact describing the source diff, changed files, inferred acceptance criteria, risk profile, suggested lanes, and validation status. A `ReviewLaneResult` is the trusted per-lane artifact consumed by synthesis. A `ReviewSynthesis` groups and deduplicates lane findings. A `FinalReviewArtifact` contains the final markdown and machine-readable result. A `PreflightManifest` records local fixture validation and must keep `remote_mutations` set to `none`.

This plan adds four schema-validated artifact types. A `ReviewLaneDraft` is an agent-authored draft that may contain findings and evidence requests but is not trusted for verification. An `EvidenceLedger` is a command-authored record that links evidence verdicts to individual draft findings. A `FeedbackManifest` records whether local feedback was applied or that the feedback phase was a no-op. A `PublishManifest` records a dry-run publication decision and must always have `remote_mutations: "none"` in this issue.

The current artifact helper is the repository-relative executable `scripts/scherzo-review`. It currently has commands for dry-run review brief generation, lane execution, synthesis, preflight, and artifact validation. The current specialist lane IDs are `correctness`, `test-quality`, `idioms-maintainability`, and `security-performance`.

The current agent harness is `scripts/scherzo_review/agent_lane_harness.py`. It contains backend-independent helpers such as input bundle retention and normalization helpers, but it also contains script-level backend code for `heuristic`, `fixture`, and `external`. The native workflow should reuse the safe artifact helpers and mark the external backend as legacy for production use, while leaving actual removal to LIV-115.

The lane prompt lookup currently maps a lane ID to a Markdown file under `scripts/scherzo_review/prompts/` by replacing hyphens with underscores. The resulting prompt paths are `scripts/scherzo_review/prompts/correctness.md`, `scripts/scherzo_review/prompts/test_quality.md`, `scripts/scherzo_review/prompts/idioms_maintainability.md`, and `scripts/scherzo_review/prompts/security_performance.md`. If any prompt file is missing when implementation begins, create it with the lane-specific instructions described in this plan.

Workflow definitions in this repository are expected under `.scherzo/workflows/`. The intended native workflow file is `.scherzo/workflows/review-native.yml`; if the repository standard uses a different extension when the structured-output platform lands, keep the same basename and adapt the extension while preserving the semantics here. The exact native structured-output keys and runner command are intentionally unresolved at draft time and must be resolved by the first milestone before implementation proceeds further.

## Preconditions and Verified Facts

The implementation must start from a clean source-control state. From the repository root, run:

    jj status --color=never

The expected safe starting point is no working-copy changes except the implementation work being performed. If other changes exist, inspect them before editing and do not overwrite unrelated work.

The repository uses `direnv` and `devenv` for the toolchain. If `direnv exec . <command>` fails because `.envrc` is blocked, inspect `.envrc`, run `direnv allow .` from the repository root, and retry through `direnv exec .`.

This plan depends on native structured agent outputs already existing in Scherzo. The required platform behavior is:

- A workflow `kind: agent` step can declare that its output must be JSON matching `docs/schemas/review-artifacts.v1.schema.json#/$defs/ReviewLaneDraft`.
- The workflow persists the raw agent output, the parsed structured output when valid, and transcript artifacts.
- If parsing or schema validation fails, the workflow still retains raw output and exposes failure metadata to downstream command steps.
- If an agent step itself fails before producing a draft, downstream normalization steps can still run and can receive a metadata artifact describing the failure.
- Workflow syntax supports the equivalent of `on_failure: continue` for native agent lanes, not only for command steps.
- Command steps can receive repository-relative paths or artifact references produced by prior steps, including failure metadata and raw-output artifacts.
- The run root or artifact root can be referenced stably from workflow YAML so lane outputs stay separated by run and by lane.
- Native agent lanes can be configured with a read-only or no-write workspace policy, can avoid inheriting GitHub and Linear write tokens, and can be prevented from running arbitrary shell commands.
- Workflow steps can run without remote mutation credentials during preflight.

The first implementation milestone must prove every item in that platform list with a minimal native workflow and must update this plan's Decision Log with the exact YAML keys, runner command, metadata paths, and containment keys. If the platform lacks any required behavior, do not continue with schema or helper expansion.

The current repository facts checked while drafting and during review incorporation are:

- `scripts/scherzo-review` exists and is the artifact helper executable.
- `scripts/scherzo-review` currently defines `dry-run`, `run-lane`, `synthesize`, `preflight`, and `validate` subcommands.
- `scripts/scherzo-review` currently defines the four specialist lane IDs listed above.
- `scripts/scherzo_review/agent_lane_harness.py` exists and contains input bundle retention helpers for `review-brief.v1.json`, `diff.patch`, `source-metadata.v1.json`, `changed-files.v1.json`, `validation-status.v1.json`, `context-manifest.v1.json`, prompt text, raw agent output, and evidence ledgers.
- `scripts/scherzo_review/agent_lane_harness.py` currently contains `AGENT_BACKENDS = {"heuristic", "fixture", "external"}`; the `external` backend is to be bypassed for native production review execution and only deprecated in this issue.
- `docs/schemas/review-artifacts.v1.schema.json` exists and defines the current artifact contract but does not define `ReviewLaneDraft`, `EvidenceLedger`, `FeedbackManifest`, or `PublishManifest`.
- `.scherzo/workflows/implementation.yaml` currently runs staged review as command steps that invoke `scripts/scherzo-review run-lane` and uses `SCHERZO_STAGED_REVIEW_AGENT_BACKEND` for backend selection. This issue leaves that workflow unchanged except for any documentation or comments explicitly needed for deprecation warnings.
- `test/review_artifacts_test.gleam` already exercises review artifact helper commands through `command_step.run` and is the right home for new command-level tests unless implementation discovers a more specific existing test module.

## Scope Boundaries

In scope:

- Prove the native structured-output workflow contract with a minimal direct-runner spike before broad helper work.
- Add or update the separate native review workflow definition under `.scherzo/workflows/`.
- Add schema definitions for native lane drafts, evidence ledgers, local feedback manifests, and dry-run publish manifests to `docs/schemas/review-artifacts.v1.schema.json`.
- Update `scripts/scherzo-review` and modules under `scripts/scherzo_review/` to prepare native lane inputs, validate native lane drafts, verify evidence, normalize lane results, synthesize final results, apply optional local feedback, validate final artifacts, run native preflight through the native workflow runner, and produce a dry-run publish manifest.
- Preserve semantic fixture coverage for PR #80, inverted auth/control-condition, static suspicion, malformed output, lane failure, no-finding scenarios, and no-mutation containment.
- Add deprecation warnings that `--agent-backend external` is legacy and not the production native review path.
- Keep dry-run and preflight commands no-side-effect and local-artifact-only.

Out of scope:

- Implementing Scherzo's core native structured-output platform.
- Posting pull-request comments, updating Linear, or mutating remote state from dry-run or preflight commands.
- Cutting over LIV-115 before native workflow validation passes.
- Integrating the native path into `.scherzo/workflows/implementation.yaml`; LIV-115 owns that integration after this issue proves readiness.
- Removing `run_external_agent`, `SCHERZO_REVIEW_AGENT_COMMAND`, external-backend-specific readiness logic, or other rollback code; LIV-115 owns actual removal after cutover.
- Changing unrelated Scherzo workflow behavior.
- Rewriting synthesis logic solely for style.

The boundary for `scripts/scherzo-review` is deliberate. It remains the artifact helper and preflight runner. It stops being responsible for launching production review agents in the native operator workflow. The external backend code in `scripts/scherzo_review/agent_lane_harness.py` should be marked legacy during native rollout and left in place as rollback until LIV-115 explicitly removes it.

## Milestones

Milestone 1 proves the platform contract. At the end of this milestone, a minimal native structured-output workflow has been run through the Scherzo workflow runner, and the plan records the exact runner command, YAML keys, run-root interpolation, raw-output artifact paths, parsed-output artifact paths, failure metadata shape, downstream continuation behavior, and read-only lane containment policy. This comes first because the full review workflow cannot safely handle failed lanes unless these platform behaviors are real.

Milestone 2 establishes the artifact contract. At the end of this milestone, the repository schema defines native lane drafts, evidence ledgers, local feedback manifests, and dry-run publish manifests, and the helper script can validate all of them without running any agents. This gives native structured output a stable schema target and gives final validation a real contract for publish and feedback artifacts.

Milestone 3 prepares the workflow shape and lane inputs. At the end of this milestone, `.scherzo/workflows/review-native.yml` contains one prepare command step, four native agent lane steps with read-only/no-remote containment, per-lane evidence command steps, per-lane normalization command steps, synthesis, apply-feedback, final validation, and dry-run publish. The workflow may not yet pass all fixtures, but the dependency graph, failure-continuation behavior, and artifact paths are explicit and use the syntax proven in Milestone 1.

Milestone 4 implements deterministic evidence verification and lane-result normalization. At the end of this milestone, a lane draft plus evidence ledger becomes a valid `ReviewLaneResult`, and malformed output or lane failure becomes a failed lane result rather than a workflow crash. Evidence verdicts verify only specific linked findings.

Milestone 5 wires synthesis, apply-feedback, final validation, and dry-run publish. At the end of this milestone, trusted lane results produce a `review_synthesis`, a `final_review`, a feedback manifest, a validation result, and a publish manifest with `remote_mutations: "none"` for dry-run and preflight.

Milestone 6 ports and expands fixture preflight. At the end of this milestone, native preflight invokes the actual `.scherzo/workflows/review-native.yml` through the Scherzo runner and preserves the PR #80, inverted auth/control-condition, static suspicion, malformed output, lane failure, no-finding, and no-mutation scenarios. The manifest must say the native workflow is cutover-ready only when every scenario passes and every lane artifact came from native agent steps.

Milestone 7 deprecates the script-level external backend from the production story without removing it. At the end of this milestone, help text and warnings tell operators that native workflow lanes are the intended production path, normal repository gates pass, and LIV-115 remains responsible for implementation-workflow integration and backend removal.

## Plan of Work

Start with the native structured-output compatibility spike. Add the smallest workflow file needed to prove the contract, preferably `.scherzo/workflows/review-native-contract-spike.yml` if the platform supports normal workflow files for spikes. The spike must include one `kind: agent` step that emits a valid minimal `ReviewLaneDraft`, one lane configured to emit malformed output, one lane or controlled failure case that simulates agent-step failure before a parsed draft exists, and one downstream `kind: command` step that reads the parsed artifact when present and the raw-output, transcript, and metadata artifacts when parsing or execution fails. Use the native platform's exact read-only/no-write syntax for the agent lanes. Run the spike with the Scherzo workflow runner, not by calling `scripts/scherzo-review run-lane`. After it passes, update this plan's Decision Log and Validation sections with the exact runner command, YAML keys, metadata fields, and artifact paths. If it cannot pass, stop and record the platform gap.

Then extend `docs/schemas/review-artifacts.v1.schema.json`. Add `ReviewLaneDraft` to the top-level `oneOf`. A draft is an agent-authored artifact with `artifact_type: "review_lane_draft"`, `schema_version: 1`, `generated_at_utc`, `producer`, `lane`, `input_refs`, `draft_findings`, `review_notes`, `evidence_requests`, `self_check`, and `remote_mutations`. Its `remote_mutations` field must be the constant `none`. Its locations must use repository-relative paths. It may include a proposed `blocking` value for triage, but normalization must not trust it.

In the same schema file, add `EvidenceLedger` to the top-level `oneOf`. A ledger has `artifact_type: "review_evidence_ledger"`, `schema_version: 1`, `generated_at_utc`, `lane_id`, `draft_ref`, `checks`, `verdicts`, `execution_status`, and `remote_mutations: "none"`. Each check records an allowlisted evidence key, the exact command or static validation performed by the command step, exit status, a concise output excerpt, and a verdict such as `verified`, `not_reproduced`, `not_applicable`, `context_only`, or `rejected`.

Define evidence requests tightly. Each `EvidenceRequest` in a `ReviewLaneDraft` must have `request_id`, `draft_finding_id`, `evidence_key`, `claim`, `expected_observation`, and a `target` object. The `target` object identifies a harness-owned test name, fixture reproduction ID, schema artifact path, changed file path, or static-scan rule; it never contains an arbitrary shell command. `verify-evidence` must ignore or reject requests that do not link to an existing draft finding. A successful command exit verifies a finding only when the observed output matches `expected_observation` for that same `request_id` and `draft_finding_id`. Generic green gates may be recorded as `context_only`; they do not verify arbitrary correctness findings.

Also add `FeedbackManifest` and `PublishManifest` to the schema. `FeedbackManifest` has `artifact_type: "review_feedback_manifest"`, `schema_version: 1`, `generated_at_utc`, `final_review_ref`, `feedback_refs`, `actions`, `result_final_review_ref`, `execution_status`, and `remote_mutations: "none"`. A no-feedback preflight writes a valid manifest with `actions: []` and a no-op status. `PublishManifest` has `artifact_type: "review_publish_manifest"`, `schema_version: 1`, `generated_at_utc`, `mode: "dry-run"`, `final_review_ref`, `published_markdown_path`, `remote_targets: []`, `execution_status`, and `remote_mutations: "none"`. This issue must reject every publish mode other than `dry-run`.

Update `scripts/scherzo-review` to add a native preparation command. Name it `prepare-native`. It should accept the same source options as `dry-run`: `--diff-file`, `--pr`, `--repo`, `--from`, `--to`, `--output-dir`, `--source-label`, and repeated `--test-status`. It should write the shared artifacts that each native lane needs under the output directory: `review-brief.v1.json`, `diff.patch`, `source-metadata.v1.json`, `changed-files.v1.json`, `validation-status.v1.json`, and `context-manifest.v1.json`. It should print the paths it wrote in a machine-readable summary so the workflow can pass them to agent steps.

Update or create lane prompts under `scripts/scherzo_review/prompts/`. Each prompt must tell the agent to inspect `diff.patch` directly, use the brief only as orientation, cite repository-relative locations only, never claim remote mutation, and emit `review_lane_draft` JSON only. The correctness prompt must explicitly say that executable evidence is required for a blocking correctness finding and that static suspicion should be expressed as a suspicion or evidence request, not as trusted proof.

Create `.scherzo/workflows/review-native.yml` using the syntax proven by the spike. Preserve this graph:

    workflow id: review-native
    prepare_review: kind command, runs scripts/scherzo-review prepare-native
    assert_clean_before_lanes: kind command, runs jj status --color=never and stores a baseline
    lane_correctness: kind agent, needs prepare_review and assert_clean_before_lanes, emits review_lane_draft
    lane_test_quality: kind agent, needs prepare_review and assert_clean_before_lanes, emits review_lane_draft
    lane_idioms_maintainability: kind agent, needs prepare_review and assert_clean_before_lanes, emits review_lane_draft
    lane_security_performance: kind agent, needs prepare_review and assert_clean_before_lanes, emits review_lane_draft
    assert_clean_after_lanes: kind command, needs all lane steps, continues after lane failure, fails on tracked-file mutation
    verify_correctness_evidence: kind command, needs lane_correctness and assert_clean_after_lanes
    verify_test_quality_evidence: kind command, needs lane_test_quality and assert_clean_after_lanes
    verify_idioms_maintainability_evidence: kind command, needs lane_idioms_maintainability and assert_clean_after_lanes
    verify_security_performance_evidence: kind command, needs lane_security_performance and assert_clean_after_lanes
    normalize_correctness: kind command, needs lane_correctness and verify_correctness_evidence
    normalize_test_quality: kind command, needs lane_test_quality and verify_test_quality_evidence
    normalize_idioms_maintainability: kind command, needs lane_idioms_maintainability and verify_idioms_maintainability_evidence
    normalize_security_performance: kind command, needs lane_security_performance and verify_security_performance_evidence
    synthesize_review: kind command, needs all normalize steps
    apply_feedback: kind command, needs synthesize_review
    final_validation: kind command, needs apply_feedback
    publish_review: kind command, needs final_validation

Each lane step must pass the same input artifact references to the agent: the review brief, diff, source metadata, changed files, validation status, and context manifest. Each lane step must use the platform's read-only or no-write policy, must not inherit GitHub or Linear write tokens, must not expose arbitrary shell execution, must continue on malformed structured output, and must write its structured draft under a lane-specific directory such as `tmp/scherzo-review-native/<run-id>/lanes/correctness/draft.v1.json` or the platform-equivalent run artifact root. If the native platform uses a different artifact directory convention, use that convention but keep lane outputs separate and repository-relative in retained artifact references.

Add `scripts/scherzo-review verify-evidence`. It must accept `--lane`, `--draft`, `--brief`, `--diff-file`, `--changed-files`, `--validation-status`, `--context-manifest`, and `--output-dir`. It must validate the draft JSON, reject absolute paths and parent-directory escapes, map agent `evidence_requests` to an allowlist of deterministic checks, run those checks, and write `evidence-ledger.v1.json`. The first allowlist should include only checks this repository can run locally: `gleam_test`, `glinter`, `scherzo_lint`, `schema_validate_artifacts`, `diff_static_scan`, and named fixture reproductions used by native preflight. Do not execute arbitrary shell commands from agent output. A verdict is `verified` only when it is linked to a request and finding and the expected observation is met.

Add `scripts/scherzo-review normalize-lane-result`. It must accept `--lane`, `--draft`, `--evidence-ledger`, `--agent-step-metadata`, `--brief`, and `--output-dir`. For a valid draft, it writes `review-lane-<lane>.v1.json` with `artifact_type: "review_lane_result"`. For missing, malformed, or failed native agent output, it writes a valid failed lane result with `execution_status.state: "failed"`, zero findings, a concise error, and artifact references to the raw output and transcript when available. For correctness findings, it must set `verified` and `blocking` from same-finding evidence ledger verdicts, not from the draft. Unverified correctness blockers become non-blocking suspicions with a recorded downgrade action.

Keep `scripts/scherzo-review synthesize` as the command that consumes final lane results. Extend it only where needed to accept the native output paths and evidence-ledger artifact references. It should continue to produce `review_synthesis` and `final_review` artifacts and continue to enforce that blocking correctness findings require verified executable evidence. It must reject direct `ReviewLaneDraft` inputs.

Add `scripts/scherzo-review apply-feedback`. It must accept `--final-review`, optional repeated `--feedback-artifact`, and `--output-dir`. In normal preflight it should run with no feedback and write `feedback-manifest.v1.json` with a no-op action list. If feedback artifacts are provided later, they must be local JSON files that adjust wording, suppress duplicate notes, or request another normalization pass. This command must not contact Linear or GitHub.

Add or update final validation through `scripts/scherzo-review validate`. It must validate every retained review artifact, assert all dry-run and preflight artifacts have `remote_mutations: "none"`, assert no lane draft is consumed directly by synthesis, assert every final lane result either has a valid evidence ledger or a failed/skipped/blocked execution status, assert publish and feedback manifests match their schemas, assert no lane mutated tracked files, and assert a preflight manifest's cutover-readiness section remains not ready when any fixture scenario fails.

Add `scripts/scherzo-review publish`. For this issue, it should support dry-run mode only. It accepts `--final-review`, `--mode dry-run`, and `--output-dir`, writes `publish-manifest.v1.json`, prints the final markdown path, and records `remote_mutations: "none"`. A future non-dry-run publish path may be enabled by LIV-115 after native validation passes; do not enable it in this issue.

Add `scripts/scherzo-review native-preflight`. It must invoke the actual `.scherzo/workflows/review-native.yml` through the Scherzo runner command proven in Milestone 1. It may wrap the runner to set fixture inputs and collect artifacts, but it must not call `scripts/scherzo-review run-lane` or otherwise simulate native agent steps in Python. It must support repeated `--scenario` and `--output-dir`. It must write `preflight-manifest.v1.json` with execution mode recorded as `native`, scenario pass/fail counts, native lane step provenance, lane run summaries, artifact paths, dirty-tree check results, and a `cutover_readiness` object.

Deprecate production use of `scripts/scherzo-review run-lane --agent-backend external` by adding help text and runtime warnings saying that the external backend is legacy and native workflow lanes are the intended production path. Do not remove `run_external_agent`, external environment variables such as `SCHERZO_REVIEW_AGENT_COMMAND`, or external-backend-specific preflight readiness conditions in this issue.

## Concrete Steps

1. From the repository root, inspect source control and environment:

       jj status --color=never
       direnv exec . ./scripts/scherzo-review validate --help

   Expect `jj status` to show only intended implementation changes, and expect the validate help command to exit 0. If `direnv` reports a blocked `.envrc`, inspect `.envrc`, run `direnv allow .`, and retry.

2. Prove the native structured-output contract before broad helper work. Create the minimal workflow file `.scherzo/workflows/review-native-contract-spike.yml` using the structured-output platform's current syntax. Include valid, malformed, and failed native agent cases plus a downstream command that reads parsed output or failure metadata. Run it with the Scherzo workflow runner. [CLARIFY] Replace this sentence with the exact runner command after the platform lands; until then, implementation must stop here rather than guessing.

3. Record the spike result in this plan before continuing. Update the Decision Log with the exact runner command, the YAML keys for schema-bound output, the key for failure continuation, the read-only/no-write lane policy, the raw-output artifact path, the transcript artifact path, the failure metadata path, and the run-root interpolation. If any required behavior is missing, stop implementation and record the platform gap in Surprises & Discoveries.

4. Commit after the contract spike passes and this plan is updated. Suggested commit content: native structured-output contract spike.

5. In `docs/schemas/review-artifacts.v1.schema.json`, add `$defs.ReviewLaneDraft`, `$defs.DraftFinding`, `$defs.EvidenceRequest`, `$defs.EvidenceLedger`, `$defs.EvidenceCheck`, `$defs.EvidenceVerdict`, `$defs.FeedbackManifest`, and `$defs.PublishManifest`. Add `ReviewLaneDraft`, `EvidenceLedger`, `FeedbackManifest`, and `PublishManifest` to the top-level `oneOf`.

6. Add schema validation examples to the helper script's existing validation path so `scripts/scherzo-review validate --artifact <path>` accepts all new artifact types and rejects drafts or manifests with absolute paths constructed by tests at runtime, parent-directory escapes such as `../secret.txt`, or non-`none` `remote_mutations`.

7. In `test/review_artifacts_test.gleam`, add `review_lane_draft_path_safety_validation_test`. The test should write one draft with a runtime-constructed absolute path, one draft with `../secret.txt`, and one draft with `remote_mutations` set to a value other than `none`; each `scripts/scherzo-review validate --artifact <draft>` command must fail and print a message containing `REVIEW_ARTIFACT_VALID=error`.

8. Commit after the schema-only work and targeted validation tests pass. Suggested commit content: artifact schema additions for native review drafts, ledgers, feedback, and publish manifests.

9. In `scripts/scherzo-review`, extract the existing `dry-run` input preparation logic into a reusable function if needed, then add the `prepare-native` parser entry and command function. It must write the shared lane input artifacts under the requested output directory.

10. Run a targeted preparation command from the repository root:

       direnv exec . ./scripts/scherzo-review prepare-native --from @- --to @ --output-dir tmp/scherzo-review-native-smoke --source-label local-smoke --test-status gleam-test=not_run:smoke-only

    Expect exit 0 and the files `tmp/scherzo-review-native-smoke/review-brief.v1.json`, `tmp/scherzo-review-native-smoke/diff.patch`, `tmp/scherzo-review-native-smoke/changed-files.v1.json`, and `tmp/scherzo-review-native-smoke/validation-status.v1.json` to exist. Then run:

       direnv exec . ./scripts/scherzo-review validate --artifact tmp/scherzo-review-native-smoke/review-brief.v1.json

    Expect exit 0.

11. Update or create the four lane prompt files under `scripts/scherzo_review/prompts/`. Keep the prompts concise and lane-specific. Each prompt must require `review_lane_draft` JSON output, direct diff inspection, repository-relative locations, no remote mutation, and no trusted correctness evidence claim.

12. Create `.scherzo/workflows/review-native.yml` with the graph described in the Plan of Work, using the exact syntax proven by `.scherzo/workflows/review-native-contract-spike.yml`. Include pre/post dirty-tree command steps and read-only/no-write containment for all native agent lanes.

13. Commit after `prepare-native` and the workflow skeleton can run far enough through the native runner to produce lane input artifacts and retained native lane metadata. Suggested commit content: native review workflow skeleton and lane input preparation.

14. Add `verify-evidence` to `scripts/scherzo-review`. Implement only the allowlisted checks named in this plan. Store command transcripts or excerpts under the lane output directory, and include artifact references in `evidence-ledger.v1.json`.

15. In `test/review_artifacts_test.gleam`, add `evidence_verdict_must_link_to_finding_test`. It should create a draft with finding `F1` and an evidence request for `F1`, then create or induce a ledger verdict for a different finding ID; normalization must not mark `F1` verified.

16. In `test/review_artifacts_test.gleam`, add `generic_gleam_test_does_not_verify_arbitrary_correctness_claim_test`. It should request a generic `gleam_test` check without a target test, fixture reproduction, or expected observation tied to the finding; the ledger may record `context_only`, but normalization must leave the correctness finding unverified and non-blocking.

17. Add `normalize-lane-result` to `scripts/scherzo-review`. It must produce a valid `review_lane_result` for successful, malformed, missing, and failed lane drafts.

18. In `test/review_artifacts_test.gleam`, add `correctness_blocker_downgraded_without_verified_reproduction_test`. The draft should propose a blocking correctness finding; the ledger should contain no verified same-finding executable evidence; the normalized lane result must contain `"blocking": false`, `"verified": false`, and a downgrade action.

19. In `test/review_artifacts_test.gleam`, add `missing_or_malformed_draft_produces_failed_lane_result_test`. The command should exit 0 and produce `review-lane-correctness.v1.json` with `execution_status.state` equal to `failed`, zero findings, and references to raw output or metadata when available.

20. Run a targeted malformed-output normalization smoke test using a small hand-written invalid raw output fixture under `tmp/scherzo-review-native-smoke/lanes/correctness/`. The command should exit 0 and produce a failed `ReviewLaneResult`, not crash:

       direnv exec . ./scripts/scherzo-review normalize-lane-result --lane correctness --draft tmp/scherzo-review-native-smoke/lanes/correctness/draft.v1.json --evidence-ledger tmp/scherzo-review-native-smoke/lanes/correctness/evidence-ledger.v1.json --agent-step-metadata tmp/scherzo-review-native-smoke/lanes/correctness/agent-step-metadata.v1.json --brief tmp/scherzo-review-native-smoke/review-brief.v1.json --output-dir tmp/scherzo-review-native-smoke/lanes/correctness

    Expect `tmp/scherzo-review-native-smoke/lanes/correctness/review-lane-correctness.v1.json` to validate and to contain `execution_status.state` equal to `failed` for malformed input.

21. Commit after evidence verification and normalization pass targeted smoke checks. Suggested commit content: deterministic evidence ledgers and native lane-result normalization.

22. Update `scripts/scherzo-review synthesize` only where needed so it can consume native lane result paths without depending on script-level lane backends. Confirm it rejects lane draft inputs and consumes only `review_lane_result` artifacts.

23. Add `apply-feedback` and dry-run `publish` commands to `scripts/scherzo-review`. The publish command for this issue must reject any mode other than `dry-run` unless LIV-115 later changes the policy.

24. In `test/review_artifacts_test.gleam`, add `publish_and_feedback_manifests_are_schema_valid_and_local_only_test`. It should run no-op feedback and dry-run publish, validate both artifacts, assert `remote_mutations` is `none`, and assert publish mode values other than `dry-run` fail.

25. Add final validation checks to `scripts/scherzo-review validate` for native preflight manifests, evidence ledgers, dry-run publish manifests, feedback manifests, direct-draft-to-synthesis misuse, same-finding evidence linkage, and dirty-tree check results.

26. Commit after synthesis, apply-feedback, validation, and dry-run publish produce valid local artifacts. Suggested commit content: native synthesis finalization and dry-run publish validation.

27. Add or update local fixture data under `scripts/scherzo_review/fixtures/` for the required scenarios. Use stable scenario IDs even if existing aliases differ: `pr-80`, `inverted-auth-control-condition`, `static-suspicion`, `malformed-agent-output`, `lane-failure`, `no-findings`, and `lane-mutates-worktree`. Do not fetch PR #80 during preflight; store a local representative diff fixture so preflight remains offline and no-side-effect.

28. Add `native-preflight` to `scripts/scherzo-review`. It must call the native workflow runner command proven in the spike, pass fixture inputs to `.scherzo/workflows/review-native.yml`, and assert scenario-specific expectations before writing `preflight-manifest.v1.json`. It must fail if artifacts lack native agent step provenance or if any lane result was produced by script-level `run-lane` execution.

29. In `test/review_artifacts_test.gleam` or a more specific workflow test module if one already exists, add `native_preflight_requires_runner_provenance_test`. It should feed a manifest that claims native mode but lacks native lane step provenance; validation must fail.

30. Run native preflight for all required scenarios:

       direnv exec . ./scripts/scherzo-review native-preflight --output-dir tmp/scherzo-review-native-preflight --scenario pr-80 --scenario inverted-auth-control-condition --scenario static-suspicion --scenario malformed-agent-output --scenario lane-failure --scenario no-findings --scenario lane-mutates-worktree
       direnv exec . ./scripts/scherzo-review validate --artifact tmp/scherzo-review-native-preflight/preflight-manifest.v1.json --require-cutover-ready

    During implementation, expect this command to fail until the `lane-mutates-worktree` scenario is correctly contained and recorded as a passed safety scenario. The final successful run must exit 0, and the manifest must report seven scenarios, seven passed scenarios, zero failed scenarios, `remote_mutations: "none"`, execution mode `native`, native lane step provenance for every agent lane, and `cutover_readiness.status` equal to `ready`.

31. Run the legacy fixture preflight once to prove migration parity remains available:

       direnv exec . ./scripts/scherzo-review preflight --agent-backend fixture --output-dir tmp/scherzo-review-fixture-preflight
       direnv exec . ./scripts/scherzo-review validate --artifact tmp/scherzo-review-fixture-preflight/preflight-manifest.v1.json --require-cutover-ready

    Expect exit 0. If this legacy manifest path differs in the current implementation, update the helper so this path is stable before relying on it in documentation.

32. Add deprecation warnings for production use of `--agent-backend external`. Do not remove external backend code, environment variables, or rollback documentation in this issue.

33. Run normal repository gates from the repository root:

       direnv exec . gleam format --check src test
       direnv exec . gleam test
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

    Expect every command to exit 0. Treat warnings as baseline inventory unless this implementation introduces new warnings.

34. Commit after all preflight and repository gates pass. Suggested commit content: native review preflight fixtures and external-backend deprecation.

35. Update this ExecPlan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective sections with the actual validation outputs and any syntax adjustments made for native structured output.

## Testing and Falsifiability

The native workflow is correct only if it can be falsified by local scenarios and targeted tests. Implement the fixture scenarios as executable preflight cases, not as passive sample files. In addition to fixture preflight, add unit-style command tests in `test/review_artifacts_test.gleam` unless implementation discovers a more specific existing test module.

The platform-contract spike is the first falsification point. It must prove that `.scherzo/workflows/review-native-contract-spike.yml` can be invoked through the Scherzo workflow runner, that a native agent step can emit valid schema-bound JSON, that malformed JSON retains raw output and failure metadata, that an agent-step failure still allows downstream normalization, and that the read-only/no-write lane policy is enforced or detectable. The plan is disproved if the spike only exercises helper commands or if downstream steps cannot read failure artifacts.

The PR #80 fixture must use a local diff fixture representing the historical review case. The expected behavior is that all four native lanes run through `.scherzo/workflows/review-native.yml`, at least one meaningful review artifact is produced, synthesis succeeds, final markdown is generated, native lane step provenance is recorded, and no remote mutations occur. This scenario proves the end-to-end happy path on a realistic review.

The inverted auth/control-condition fixture must cause the correctness lane to propose a correctness finding and must provide executable evidence that the command step can verify for that same draft finding. The expected final result is a blocking correctness finding with `verified: true`, `evidence_type` equal to `test`, `runtime`, or `reproduction`, and an evidence ledger verdict whose `draft_finding_id` matches the normalized finding.

The static suspicion fixture must cause the correctness lane to propose a suspicious control-flow or authorization finding without executable proof. The expected final result is not a blocking correctness finding. The normalizer or synthesis must downgrade it to a non-blocking suspicion and record a downgrade action explaining that executable evidence was missing.

The malformed-output fixture must make one native lane emit invalid JSON or JSON that fails the `ReviewLaneDraft` schema. The expected final result is a valid failed `ReviewLaneResult` for that lane, a final review artifact that lists the lane failure in `execution_issues`, retained raw output and metadata artifacts, and no crash of the workflow.

The lane-failure fixture must simulate an agent step failing before producing a draft. The expected final result is the same containment shape as malformed output: one failed lane status, no fabricated findings from that lane, synthesis completed with lane failures, and a final review artifact that remains valid.

The no-finding fixture must use a benign diff. The expected final result is all lanes succeeded, `finding_counts.total` equal to 0, `finding_counts.blocking` equal to 0, final markdown that states no findings were found, and `remote_mutations: "none"` throughout.

The lane-mutation fixture must simulate or force a lane attempting to modify a tracked file. The expected result is that the post-lane dirty-tree check detects the mutation, validation records the safety scenario as passed because the mutation was contained or rejected, and cutover readiness remains not ready if a real native run can mutate tracked files without detection.

Targeted tests must cover path and mutation safety. Add assertions that a draft with an absolute local path constructed at test runtime, a parent-directory path such as `../secret.txt`, or `remote_mutations` other than `none` is rejected before synthesis. Do not hardcode a checkout-specific absolute path in fixtures or plan text.

Targeted tests must cover evidence semantics. Add assertions that a ledger verdict for one finding cannot verify another finding, that generic green `gleam_test` output is `context_only` unless tied to a target test or reproduction and expected observation, that an unverified correctness blocker is downgraded, and that a named fixture reproduction verifies only the draft finding whose request it satisfies.

Targeted tests must cover artifact contracts. Add assertions that `FeedbackManifest` and `PublishManifest` validate through `scripts/scherzo-review validate --artifact <artifact-path>`, that publish mode values other than `dry-run` are rejected in this issue, that synthesis rejects direct `ReviewLaneDraft` inputs, and that a preflight manifest claiming native execution without native lane step provenance is invalid.

Run targeted artifact validation after each generated JSON artifact:

    direnv exec . ./scripts/scherzo-review validate --artifact <artifact-path>

Run the native preflight command for all required scenarios as shown in the Concrete Steps. The plan is disproved if any scenario passes while skipping the native workflow runner, if a draft is consumed directly by synthesis, if a static correctness suspicion remains blocking without same-finding executable evidence, if malformed output crashes the workflow, if a lane failure stops downstream normalization, if any preflight artifact records remote mutation, or if a lane can mutate tracked files without detection.

## Validation and Acceptance

Validation has four levels.

First, validate the native structured-output platform contract. From the repository root, run the local Scherzo workflow-runner command against `.scherzo/workflows/review-native-contract-spike.yml`:

    direnv exec . gleam run -- workflow run .scherzo/workflows/review-native-contract-spike.yml --run-root tmp/scherzo-native-contract-smoke --run-id native-contract-smoke --native-review-scenario contract-spike

Expected result: the command exits 0, prints `SCHERZO_WORKFLOW_RUN=ok`, writes `tmp/scherzo-native-contract-smoke/native-runner-summary.v1.json`, records one valid parsed draft, one malformed-output case with retained raw output and metadata, one failed-agent case with retained failure metadata, downstream command output proving it read those artifacts, and dirty-tree containment artifacts.

Second, run native fixture preflight from the repository root. The wrapper is acceptable only if it invokes `.scherzo/workflows/review-native.yml` through the Scherzo runner and validates native lane step provenance:

    direnv exec . ./scripts/scherzo-review native-preflight --output-dir tmp/scherzo-review-native-preflight --scenario pr-80 --scenario inverted-auth-control-condition --scenario static-suspicion --scenario malformed-agent-output --scenario lane-failure --scenario no-findings --scenario lane-mutates-worktree
    direnv exec . ./scripts/scherzo-review validate --artifact tmp/scherzo-review-native-preflight/preflight-manifest.v1.json --require-cutover-ready

Expected result: both commands exit 0. The manifest reports seven scenarios, all passed, no remote mutations, native execution mode, native lane step provenance for all four lanes, valid lane result paths for all lanes, evidence ledgers for all successful lane normalizations, retained raw-output or metadata paths for failed lanes, clean dirty-tree checks, and `cutover_readiness.status` equal to `ready`.

Third, run a normal local review dry-run through the native workflow runner, not by simulating lanes in the helper:

    direnv exec . gleam run -- workflow run .scherzo/workflows/review-native.yml --run-root tmp/scherzo-review-native-local --run-id native-local --native-review-scenario pr-80

Expected result: the command exits 0, prints `SCHERZO_WORKFLOW_RUN=ok`, writes `tmp/scherzo-review-native-local/native-runner-summary.v1.json`, records native lane step IDs, and writes valid local artifacts only.

Fourth, run normal repository gates:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

Expected result: all commands exit 0. The production lint gates are required; do not add production `let assert`, `panic`, or `todo` to satisfy this plan.

The implementation is accepted only when the native workflow has separate review lane agent steps, read-only/no-remote lane containment, evidence verification command steps, lane-result normalization, synthesis, apply-feedback, final validation, and dry-run publish; `--agent-backend external` is marked legacy and is not presented as a production native review path; all required fixtures pass through the native workflow runner; publish and feedback manifests are schema-valid; and LIV-115 remains blocked until this native validation has passed.

## Rollout, Recovery, and Idempotence

Roll out in parallel with the existing helper path. This issue adds `.scherzo/workflows/review-native.yml` as a separate operator workflow and does not change `.scherzo/workflows/implementation.yaml` to call the native path. Use a separate output directory such as `tmp/scherzo-review-native-preflight` or the Scherzo run artifact root so native validation cannot overwrite legacy preflight artifacts.

During migration, operators can recover by using the existing local helper commands and by leaving `SCHERZO_STAGED_REVIEW_AGENT_BACKEND` behavior unchanged in `.scherzo/workflows/implementation.yaml`. Because dry-run and preflight write only local files under `tmp/` or run artifacts, cleanup is idempotent: removing `tmp/scherzo-review-native-*` directories and rerunning the commands should produce fresh artifacts.

Do not remove external-backend code in this issue. The rollback path is to keep the existing command-lane review path available while the native operator workflow is proven. If a regression appears after adding native workflow support, disable or ignore `.scherzo/workflows/review-native.yml`, keep LIV-115 blocked, and use the legacy fixture preflight artifacts for diagnosis.

External backend deprecation should be additive: help text and warnings may say it is legacy and not the native production path, but `run_external_agent`, `SCHERZO_REVIEW_AGENT_COMMAND`, and external-backend-specific readiness checks remain until LIV-115. LIV-115 may remove them only after native preflight reports `cutover_readiness.status: "ready"`, normal repository gates pass, the implementation workflow integration is proven, and an operator approves the remote mutation policy.

Publish remains dry-run-only in this issue. A future LIV-115 cutover may enable remote publication, but only after native preflight is ready, direct native runner validation passes, and an operator explicitly approves the remote mutation policy. Until then, all manifests must say `remote_mutations: "none"`.

## Artifacts and Notes

The following inspected facts shaped this plan:

    scripts/scherzo-review
      Existing executable helper for dry-run, run-lane, synthesize, preflight, and validate.

    scripts/scherzo_review/agent_lane_harness.py
      Existing helper module for input bundle retention, fixture and external backend execution, response normalization, evidence handling, and cutover-readiness checks.

    docs/schemas/review-artifacts.v1.schema.json
      Existing schema for review_brief, review_finding, review_lane_result, review_synthesis, final_review, and preflight_manifest.

    .scherzo/workflows/implementation.yaml
      Existing implementation workflow that runs staged review lanes as command steps through scripts/scherzo-review run-lane and leaves integration to LIV-115.

    test/review_artifacts_test.gleam
      Existing Gleam test module that exercises review artifact helper commands and should receive the new command-level tests unless a more specific module exists.

    jj status --color=never
      The drafting workspace started with no working-copy changes.

Expected native artifact tree for one run, using the illustrative `tmp/` path when not running under a Scherzo run artifact root:

    tmp/scherzo-review-native/<run-id>/review-brief.v1.json
    tmp/scherzo-review-native/<run-id>/diff.patch
    tmp/scherzo-review-native/<run-id>/changed-files.v1.json
    tmp/scherzo-review-native/<run-id>/validation-status.v1.json
    tmp/scherzo-review-native/<run-id>/context-manifest.v1.json
    tmp/scherzo-review-native/<run-id>/contract/native-runner-command.txt
    tmp/scherzo-review-native/<run-id>/lanes/correctness/draft.v1.json
    tmp/scherzo-review-native/<run-id>/lanes/correctness/raw-output.txt
    tmp/scherzo-review-native/<run-id>/lanes/correctness/transcript.txt
    tmp/scherzo-review-native/<run-id>/lanes/correctness/agent-step-metadata.v1.json
    tmp/scherzo-review-native/<run-id>/lanes/correctness/evidence-ledger.v1.json
    tmp/scherzo-review-native/<run-id>/lanes/correctness/review-lane-correctness.v1.json
    tmp/scherzo-review-native/<run-id>/review-synthesis.v1.json
    tmp/scherzo-review-native/<run-id>/final-review.v1.json
    tmp/scherzo-review-native/<run-id>/feedback-manifest.v1.json
    tmp/scherzo-review-native/<run-id>/publish-manifest.v1.json
    tmp/scherzo-review-native/<run-id>/preflight-manifest.v1.json

All artifact paths stored inside JSON must be repository-relative or output-directory-relative. Do not store checkout-specific absolute paths. Tests that need an absolute path for rejection must construct it at runtime and must not hardcode a local checkout prefix.

## Interfaces and Dependencies

`ReviewLaneDraft` must have this semantic shape in `docs/schemas/review-artifacts.v1.schema.json`:

    {
      "schema_version": 1,
      "artifact_type": "review_lane_draft",
      "generated_at_utc": "...",
      "producer": { "name": "native-scherzo-agent", "version": "...", "mode": "native" },
      "lane": { "id": "correctness", "name": "Correctness reviewer", "category": "correctness", "version": "1" },
      "input_refs": [{ "artifact_type": "review_brief", "path": "...", "sha256": "..." }],
      "draft_findings": [
        {
          "draft_finding_id": "F1",
          "title": "...",
          "claim": "...",
          "severity": "high",
          "proposed_blocking": true,
          "locations": [{ "path": "src/example.gleam", "line": 10 }],
          "evidence_request_ids": ["E1"]
        }
      ],
      "review_notes": [],
      "evidence_requests": [
        {
          "request_id": "E1",
          "draft_finding_id": "F1",
          "evidence_key": "fixture_reproduction",
          "claim": "The changed control condition allows unauthorized access.",
          "expected_observation": "fixture inverted-auth-control-condition fails before the fix and passes after reverting the change",
          "target": { "fixture_id": "inverted-auth-control-condition" }
        }
      ],
      "self_check": { "inspected_diff": true, "used_repository_relative_paths": true },
      "remote_mutations": "none"
    }

`EvidenceLedger` must have this semantic shape:

    {
      "schema_version": 1,
      "artifact_type": "review_evidence_ledger",
      "generated_at_utc": "...",
      "lane_id": "correctness",
      "draft_ref": { "artifact_type": "review_lane_draft", "path": "...", "sha256": "..." },
      "checks": [
        {
          "check_id": "C1",
          "request_id": "E1",
          "draft_finding_id": "F1",
          "evidence_key": "fixture_reproduction",
          "command": "scripts/scherzo-review reproduce-fixture --scenario inverted-auth-control-condition",
          "exit_status": 0,
          "output_excerpt": "expected failure reproduced",
          "remote_mutations": "none"
        }
      ],
      "verdicts": [
        {
          "request_id": "E1",
          "draft_finding_id": "F1",
          "verdict": "verified",
          "evidence_type": "reproduction",
          "claim_supported": "The changed control condition allows unauthorized access.",
          "matched_expected_observation": true,
          "check_id": "C1"
        }
      ],
      "execution_status": { "state": "succeeded", "summary": "..." },
      "remote_mutations": "none"
    }

The `verify-evidence` command owns evidence verdicts. It may run these fixed commands from the repository root when requested by allowlisted evidence keys:

    direnv exec . gleam test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint
    direnv exec . ./scripts/scherzo-review validate --artifact <artifact-path>

It must not execute arbitrary command strings supplied by an agent. For fixture-specific reproductions, implement named local reproductions inside the helper and record the exact command that the helper ran. A whole-repository command that exits 0 is `context_only` unless the request names the target test, fixture reproduction, schema artifact, static-scan rule, or expected output that connects the command to one finding. A correctness finding can be `blocking: true` only when a same-finding ledger verdict is `verified` and `evidence_type` is `test`, `runtime`, or `reproduction`.

`FeedbackManifest` must have this semantic shape:

    {
      "schema_version": 1,
      "artifact_type": "review_feedback_manifest",
      "generated_at_utc": "...",
      "final_review_ref": { "artifact_type": "final_review", "path": "...", "sha256": "..." },
      "feedback_refs": [],
      "actions": [],
      "result_final_review_ref": { "artifact_type": "final_review", "path": "...", "sha256": "..." },
      "execution_status": { "state": "succeeded", "summary": "no feedback supplied" },
      "remote_mutations": "none"
    }

`PublishManifest` must have this semantic shape:

    {
      "schema_version": 1,
      "artifact_type": "review_publish_manifest",
      "generated_at_utc": "...",
      "mode": "dry-run",
      "final_review_ref": { "artifact_type": "final_review", "path": "...", "sha256": "..." },
      "published_markdown_path": "tmp/scherzo-review-native/<run-id>/final-review.md",
      "remote_targets": [],
      "execution_status": { "state": "succeeded", "summary": "dry run only" },
      "remote_mutations": "none"
    }

The native workflow depends on the core structured-output feature, but not on the external backend. The workflow must retain raw agent output and transcripts using native Scherzo artifact support. It must not require `SCHERZO_REVIEW_AGENT_COMMAND`, GitHub write tokens, Linear tokens, or SSH credentials for dry-run or preflight. It must use the platform's strongest read-only or no-write lane configuration and must fail validation if tracked files change during native review lanes.

## Open Questions and Clarifications Resolved

There are no unresolved plan-blocking clarifications for this issue.

The exact native runner command is now `direnv exec . gleam run -- workflow run <workflow.yml> --run-root <dir> --run-id <id> --native-review-scenario <id>`. Native structured-output YAML keys used by the checked-in workflows are `structured_output.format: json`, `artifact_name`, `required`, and an inline `schema` object.

The current workflow platform still does not expose a portable workflow-level read-only/no-write capability key for `dogfood-jj`, so this issue uses the available containment: fixture native agent steps, no remote credentials for local runner/preflight, explicit `remote_mutations: "none"` manifests, pre/post `jj status --color=never` dirty-tree checks, validation of mutation containment, and retained artifact provenance. LIV-115 should revisit stronger workspace capabilities before production cutover.

The native preflight uses `pr-80` as the stable local scenario ID and validates it through `.scherzo/workflows/review-native.yml` via the local runner.
