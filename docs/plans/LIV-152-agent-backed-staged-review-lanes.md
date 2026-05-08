# Add agent-backed staged review lanes before cutover

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo's staged review workflow is intended to replace the normal review path for code changes. Operators need that replacement to catch real semantic bugs before the cutover, not merely to classify deterministic style signals. After this plan is implemented, the staged review lanes will be backed by specialist agents that inspect the actual diff and relevant repository context, produce schema-valid `ReviewLaneResult` artifacts, retain their evidence, and fail safely when an agent lane cannot complete.

The observable result is that a local dry-run review can be exercised with `scripts/scherzo-review preflight --agent-backend fixture --output-dir tmp/scherzo-review-preflight`. The generated preflight manifest must record the backend used for every lane, must include a cutover-readiness decision, and must prove that the fixture-backed agent harness path ran instead of the old heuristic path. A deliberately adversarial fixture with an inverted authorization or control condition must produce a blocking correctness finding only when the harness itself has run and retained executable evidence from a test, runtime check, or reproduction. Static suspicions, risk notes, and coverage notes remain non-blocking review notes rather than findings.

The external-agent backend is a production integration point, not a prerequisite for deterministic validation. The shared harness will define and test the external command contract, timeout handling, credential stripping, output retention, and mutation detection, but the default staged review backend must remain safe until LIV-115 explicitly consumes a passing cutover-readiness gate.

## Problem Framing and Constraints

The current staged review helper is structurally useful, but it is a first-version deterministic reviewer. It can generate review briefs, lane result artifacts, synthesis artifacts, final review artifacts, logs, and local preflight coverage, but deterministic path and token heuristics can miss semantic failures. The motivating example is an inverted authorization or control condition: code can compile, tests can appear present, and no simple heuristic may flag that denied users are allowed or authorized users are rejected. Because the future LIV-115 cutover removes a separate human review as the final semantic safety net, the lanes themselves must become true semantic reviewers.

This plan is constrained by safety. The review workflow must remain dry-run and must not post GitHub PR comments, update Linear, push branches, or mutate remote state. The implementation must preserve the existing artifact contract in `docs/schemas/review-artifacts.v1.schema.json`, including `ReviewBrief`, `ReviewLaneResult`, `ReviewSynthesis`, and `FinalReviewArtifact`. The implementation must keep lane failures isolated and diagnosable so one broken specialist does not erase the evidence produced by other lanes. LIV-115 remains blocked until the child implementation issues under LIV-151 have landed and their validation passes.

This planning issue does not implement the lane runtime. It only defines the architecture, sequencing, tests, and validation that later child issues should implement.

## Strategy Overview

Keep the existing `scripts/scherzo-review` command line and artifact schema as the compatibility boundary. Add a shared agent-lane harness behind the existing `run-lane` command and add explicit backend selection to `preflight` so validation can choose the fixture-backed agent path instead of accidentally exercising the old heuristic path. The harness will assemble an input bundle for a lane, invoke a configured backend, normalize the backend's raw output into the repository's `ReviewLaneResult` schema, enforce lane-specific safety rules, retain transcripts and evidence artifacts, and write a schema-valid success or failure result.

The most important safety rule is that executable correctness evidence is harness-owned. An agent may propose a finding and may request a verification command or cite a scenario evidence key, but the harness must be the component that runs or certifies the allowed local test, runtime command, or reproduction. The harness captures the command, repository-relative working directory, exit code, stdout, stderr, pre-run and post-run working-tree state, file hashes, and an evidence id in `evidence-ledger.v1.json`. A blocking correctness finding may reference only a harness-issued evidence id whose evidence type is `test`, `runtime`, or `reproduction`; otherwise the harness downgrades the claim into a `review_notes` entry.

This is the right size because it reuses the existing review artifacts, preflight command, synthesis command, and validation command. The plan does not replace the whole review workflow or require LIV-115 to cut over early. It makes the smallest boundary that turns each lane from a deterministic heuristic into a semantic specialist while preserving local dry-run behavior and rollback to the existing deterministic implementation until the agent-backed lanes are proven.

The implementation should be split into child issues under LIV-151 in this order: child issue creation and shared harness, correctness, test-quality, idioms and maintainability, security and performance, and finally cutover gating work that LIV-115 consumes. Each child issue should leave the repository green, should add deterministic fixtures that exercise the new behavior without remote mutation, and should update this ExecPlan when it changes the contract.

## Alternatives Considered

The simplest alternative is to keep improving deterministic heuristics in `scripts/scherzo-review`. That is insufficient because the known failure mode is semantic: an inverted authorization condition can look syntactically ordinary and can require understanding expected behavior across a diff, tests, and nearby source files. More token rules would reduce noise for known patterns but would not provide adversarial reasoning.

A second alternative is to rely on a final human reviewer after the staged pipeline runs. That is explicitly out of scope for the cutover path. The lanes must be strong enough to stand alone because LIV-115 is a workflow cutover, not an additional advisory layer.

A third alternative is to call one generic review agent and synthesize its output into all categories. That is too coarse for this repository's review contract. The existing artifacts already model specialist lanes, lane failures, and synthesis. Keeping specialist lanes allows correctness evidence policy, test-quality coverage notes, maintainability findings, and security or performance risk notes to have different prompts, acceptance criteria, and failure handling.

## Risks and Countermeasures

The main risk is that an agent emits confident but unproven blockers. The countermeasure is a hard, harness-owned correctness evidence gate. A blocking correctness finding must have `verified: true`, `evidence_type` equal to `test`, `runtime`, or `reproduction`, and at least one `evidence_id` issued by the harness in `evidence-ledger.v1.json`. The ledger entry must record the local command or reproduction that the harness ran, its repository-relative working directory, exit code, captured stdout and stderr artifact paths, hashes, timeout status, and clean pre-run and post-run working-tree state. Static-only correctness concerns, agent-declared proof without a ledger entry, or proof that points only to raw agent output must become `review_notes` with `kind` such as `risk_note` or `follow_up_test`, not blocking findings.

Another risk is that an agent hallucinates file paths or critiques code it did not inspect. The harness must provide the real diff, changed-file metadata, current file snapshots where available, and validation status in a lane input bundle. It must require every finding or review note to cite repository-relative locations from the diff or collected context, and it must retain the agent transcript or raw output so reviewers can diagnose unsupported claims. The normalizer must reject artifact references that are absolute, point outside the lane output directory, or point to files that were not retained and hashed by the harness.

A third risk is that an agent lane fails, times out, mutates the checkout, or returns malformed JSON. The countermeasure is isolation and fail-closed readiness. Each lane runs in its own output directory. A failed lane writes a schema-valid `ReviewLaneResult` with `execution_status.state` set to `failed`, empty `findings`, retained logs, and an error summary. Synthesis records an execution issue and the final review makes the lane failure visible. The preflight manifest must include a cutover-readiness object that is `ready: false` when a required non-negative readiness scenario has a failed correctness or security/performance lane, when the backend is `heuristic`, when lane backend metadata is missing, or when required semantic fixtures did not run.

A fourth risk is remote mutation or local workspace mutation by an external backend. Prompt instructions are not a safety boundary. The harness must launch external commands with an explicit environment that removes mutation-capable and credential-bearing variables such as `GITHUB_TOKEN`, `GH_TOKEN`, `LINEAR_API_KEY`, `SCHERZO_AGENT_LINEAR_API_KEY`, and `SSH_AUTH_SOCK`; must use a fixed timeout; must capture `jj status --color=never` or an equivalent clean/dirty check before and after the backend runs; and must fail the lane if the working tree changes. The external backend may write only inside its lane output directory. Retained artifact paths that are absolute or escape the lane directory are rejected. This plan does not claim true OS-level sandboxing; external mode remains disabled by default until LIV-115 explicitly accepts that residual risk and the readiness gate passes.

A fifth risk is nondeterministic tests caused by live model behavior. The countermeasure is to separate the harness from the production agent backend. Tests and preflight regression fixtures use a deterministic fixture backend that exercises the same input bundle, output normalization, evidence policy, backend manifest recording, and artifact retention. The production backend is configured separately and is only allowed to change the semantic reviewer, not the artifact contract.

A sixth risk is false validation caused by exercising the old heuristic path. The countermeasure is explicit backend plumbing through `preflight`, manifest recording of the selected backend for every lane, tests that fail before `preflight --agent-backend fixture` is implemented, and final acceptance commands that require fixture-backed cutover readiness. A preflight run that does not record `agent_backend: fixture` or `agent_backend: external` for the agent-backed lanes is not acceptable evidence for LIV-115.

## Progress

- [x] (2026-05-08 00:00Z) Drafted this ExecPlan from the LIV-152 ticket and current repository facts.
- [x] (2026-05-08 00:00Z) Incorporated adversarial review findings about harness-owned evidence, preflight backend selection, external backend safety, cutover readiness, fixture specificity, and child issue executability.
- [ ] Create repository-relative child issue description files under `tmp/linear-liv-152-child-issues/` using the titles and acceptance snippets in this plan.
- [ ] Create the LIV-151 child issue for the shared agent-lane harness, backend plumbing, artifact retention, and cutover-readiness manifest.
- [ ] Create the LIV-151 child issue for the correctness semantic lane, concrete inverted-control fixture, and harness-owned executable-evidence gate.
- [ ] Create the LIV-151 child issue for the test-quality semantic lane.
- [ ] Create the LIV-151 child issue for the idioms and maintainability semantic lane.
- [ ] Create the LIV-151 child issue for the security and performance semantic lane.
- [ ] Create the LIV-151 child issue for the LIV-115 cutover readiness integration point and default-backend switch.
- [ ] Add a failing Gleam test that `run-lane --agent-backend fixture` writes bundle, raw output, transcript, and schema-valid lane artifacts.
- [ ] Add `scripts/scherzo_review/__init__.py` and `scripts/scherzo_review/agent_lane_harness.py` with the first pure helpers for backend parsing, artifact hashing, and repository-relative path validation.
- [ ] Wire `--agent-backend heuristic|fixture|external` through `scripts/scherzo-review run-lane` while keeping `heuristic` as the default.
- [ ] Implement lane input bundle creation with `diff.patch`, source metadata, changed-file metadata, validation status, context manifest, prompt, and file hashes.
- [ ] Implement deterministic fixture backend invocation and raw output retention.
- [ ] Implement external backend command contract, timeout handling, credential stripping, output retention, path containment, and working-tree mutation detection while keeping external mode disabled unless configured.
- [ ] Implement normalization from fixture or external raw output into `ReviewLaneResult` and failed lane artifacts.
- [ ] Implement harness-owned evidence execution and `evidence-ledger.v1.json` before allowing blocking correctness findings.
- [ ] Add `preflight --agent-backend`, propagate it to internal lane runs, and record the backend per lane in `preflight-manifest.v1.json`.
- [ ] Add `cutover_readiness` computation to the preflight manifest and `validate --require-cutover-ready` support.
- [ ] Add the concrete `inverted-auth-control-condition` fixture, reproduction script, expected output, evidence ledger assertions, and final-review blocker assertions.
- [ ] Add the `auth-control-static-suspicion-without-repro` fixture and assert that it creates a correctness review note rather than a blocker.
- [ ] Implement and validate the test-quality lane fixtures.
- [ ] Implement and validate the idioms and maintainability lane fixtures.
- [ ] Implement and validate the security and performance lane fixtures.
- [ ] Locate or add the LIV-115 staging/cutover backend configuration surface, keep its default safe, and prove it refuses cutover without readiness.
- [ ] Run the final fixture-backed validation suite and keep LIV-115 blocked until every child issue is complete.

## Surprises & Discoveries

- Observation: `scripts/scherzo-review` already provides the local no-side-effect review workflow surface: `dry-run`, `run-lane`, `synthesize`, `preflight`, and `validate`.
  Evidence: The script writes review artifacts under `tmp/` by default and prints `REVIEW_REMOTE_MUTATIONS=none` for dry-run, synthesis, and preflight commands.

- Observation: The repository already has a schema with first-class `ReviewBrief`, `ReviewLaneResult`, `review_notes`, lane execution status, synthesis, final review, and `remote_mutations: none` contracts.
  Evidence: `docs/schemas/review-artifacts.v1.schema.json` defines `ReviewLaneResult.findings`, optional `ReviewLaneResult.review_notes`, `ReviewLaneExecutionStatus`, `ReviewSynthesis.remote_mutations`, and `FinalReviewArtifact.remote_mutations`.

- Observation: The current correctness lane already recognizes that blocking correctness findings require executable evidence, but it is still deterministic and static-signal based.
  Evidence: `scripts/scherzo-review` defines `EXECUTABLE_CORRECTNESS_EVIDENCE_TYPES = {"test", "runtime", "reproduction"}` and `normalize_correctness_blockers`, while `run_specialist_lane` dispatches to deterministic lane functions.

- Observation: Existing preflight coverage already exercises no-meaningful-findings, PR #80-inspired precision, lane failure, malformed lane output, empty findings, and duplicate or conflicting synthesis scenarios.
  Evidence: `scripts/scherzo-review` defines these cases in `preflight_scenarios()` and runs them through `dry-run`, `run-lane`, artifact validation, and `synthesize`.

## Decision Log

- Decision: Preserve `scripts/scherzo-review` as the operator-facing CLI and preserve `docs/schemas/review-artifacts.v1.schema.json` as the artifact compatibility boundary.
  Rationale: The current workflow, tests, and future LIV-115 cutover can continue to consume the same commands and artifact types while the lane internals become agent-backed.
  Date: 2026-05-08

- Decision: Implement one common agent-lane harness and then specialize prompts and policies per lane.
  Rationale: Prompt assembly, context capture, transcript retention, schema validation, failure artifacts, no-remote-mutation safeguards, backend metadata, and evidence enforcement are shared concerns. Duplicating them in each lane would make failures inconsistent and harder to diagnose.
  Date: 2026-05-08

- Decision: Treat verified executable evidence as mandatory for blocking correctness findings, and make that evidence harness-owned rather than agent-declared.
  Rationale: Static semantic suspicions can be valuable, but without a test, runtime observation, or reproduction run or certified by the harness they are not reliable enough to block a cutover review path with no final human semantic gate.
  Date: 2026-05-08

- Decision: Add explicit backend selection to `preflight` and require fixture-backed preflight for acceptance.
  Rationale: The default backend remains `heuristic` during migration. Without explicit `preflight --agent-backend fixture` plumbing and manifest recording, acceptance could pass through the old deterministic lane path and fail to validate the agent harness.
  Date: 2026-05-08

- Decision: Use deterministic fixture agents for tests and preflight regression coverage while supporting a disabled-by-default configurable external agent backend for real lane execution.
  Rationale: The harness and artifact policy must be testable without network calls, credentials, or model nondeterminism. The external backend can then be enabled through a documented command contract without changing the artifact schema.
  Date: 2026-05-08

- Decision: Define the external backend contract in the shared harness child issue, but do not make external mode the default in this plan.
  Rationale: Timeout behavior, raw-output semantics, credential stripping, path containment, and working-tree mutation detection are safety-critical and can be implemented now. The exact production command, model, and credential handoff still require stakeholder confirmation before default enablement.
  Date: 2026-05-08

- Decision: Add a cutover-readiness gate to `preflight-manifest.v1.json` and `scripts/scherzo-review validate --require-cutover-ready`.
  Rationale: Lane failures and backend mistakes must be machine-readable by LIV-115. Merely surfacing failures in synthesis is not enough if a later workflow can ignore them.
  Date: 2026-05-08

- Decision: Keep LIV-115 blocked until all agent-backed lane child issues have landed and fixture-backed preflight proves the semantic fixtures with cutover readiness.
  Rationale: A workflow cutover without a human final reviewer is unsafe until the lanes themselves catch the known semantic failure mode and fail closed when they cannot run.
  Date: 2026-05-08

- Decision: Keep child issue creation in scope and make it executable through the repo-local Linear CLI.
  Rationale: This planning issue exists to create the implementation breakdown under LIV-151. The child issues must have consistent acceptance criteria instead of relying on future implementers to infer scope from this plan.
  Date: 2026-05-08

- Decision: Name this plan `docs/plans/LIV-152-agent-backed-staged-review-lanes.md`.
  Rationale: The Linear description mentions a LIV-151-prefixed path, but the active workflow contract requires the new plan file to be named with the LIV-152 issue identifier.
  Date: 2026-05-08

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo's current staged review helper is the executable Python script `scripts/scherzo-review`. It is intentionally outside the Gleam runtime and is used for local dogfood review workflows. It can generate a `ReviewBrief` from a unified diff, run one of several specialist lanes, synthesize lane results, validate artifacts, and run a local preflight suite. The existing specialist lane ids are `correctness`, `test-quality`, `idioms-maintainability`, and `security-performance`.

A `ReviewBrief` is a JSON artifact that summarizes a change. In the current implementation it includes source metadata, a diff hash, changed file count, changed areas, inferred acceptance criteria, risk profile, suggested review lanes, and test or build status supplied through `--test-status` values.

A `ReviewLaneResult` is a JSON artifact produced by one review lane. It contains lane metadata, an execution status, zero or more findings, optional review notes, and artifact references. Findings are concrete review outputs with category, severity, evidence type, verification status, blocking status, locations, details, and suggested fixes. Review notes are non-blocking observations such as risk notes, coverage notes, follow-up test requests, or general review notes.

`docs/schemas/review-artifacts.v1.schema.json` defines the artifact contract for `ReviewBrief`, `ReviewFinding`, `ReviewLaneResult`, `ReviewSynthesis`, and `FinalReviewArtifact`. The schema allows additional properties on several objects, so the agent harness can add evidence metadata and context references while preserving compatibility.

`test/review_artifacts_test.gleam` exercises the script from Gleam tests. Existing tests generate local diffs, run `scripts/scherzo-review dry-run`, run specialist lanes, validate artifacts, and assert that generated JSON and logs contain expected contract fields.

`docs/review-artifacts.md` is the human-facing artifact documentation surface and should be updated by the future implementation when the agent-backed behavior becomes real. This plan does not edit that document.

## Preconditions and Verified Facts

The working copy was clean when this plan was drafted, as shown by `jj status --color=never` reporting no changes.

`docs/plans/LIV-152-agent-backed-staged-review-lanes.md` did not already exist when this plan was drafted.

`SPECIALIST_LANES` in `scripts/scherzo-review` currently defines these lanes: `correctness`, `test-quality`, `idioms-maintainability`, and `security-performance`.

`build_review_brief` in `scripts/scherzo-review` writes a `review_brief` artifact with `source`, `implementation_summary`, `changed_areas`, `inferred_acceptance_criteria`, `risk_profile`, `suggested_review_lanes`, `test_build_status`, and notes that the dry-run did not mutate remote state.

`run_lane_command` currently loads a brief, parses the diff source, dispatches to `run_specialist_lane`, writes a lane analysis artifact, writes a lane log, writes `review-lane-<lane>.v1.json`, validates the lane result, and prints paths and counts.

`write_failed_lane_artifacts` already writes schema-valid failed lane artifacts when `run-lane` raises a `ReviewError`.

`preflight_command` already writes `tmp/scherzo-review-preflight/preflight-manifest.v1.json` by default, tracks scenario results, and prints `REVIEW_PREFLIGHT=ok` on success. Its manifest includes `remote_mutations: none`.

`preflight_scenarios()` defines the local scenario set that future implementation will extend. `preflight_assert_scenario_expectations()` is the place to add scenario-specific assertions for semantic fixtures and cutover-readiness failure cases.

`validate_command` validates one artifact path and prints `REVIEW_ARTIFACT_VALID=ok` with the artifact type and schema version. This plan adds `--require-cutover-ready` to that command for preflight manifest validation.

The repo-local Linear CLI supports issue creation with `direnv exec . linear issue create --team LIV --parent LIV-152 --title <title> --description-file <path> --no-interactive`. The command's help lists `--team`, `--parent`, `--title`, `--description-file`, labels, priority, estimate, state, and `--no-interactive` options.

The required production lint gates for future implementation are:

    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

The general test command for future implementation is:

    direnv exec . gleam test

If a disposable workspace reports that `.envrc` is blocked, the implementer should inspect `.envrc`, run `direnv allow .` from the repository root, and then retry the `direnv exec . <command>` form. This is an environment setup issue, not a test failure.

## Scope Boundaries

In scope for the future implementation are the shared agent-lane harness, lane input bundle generation, changed-file and repository context collection, agent prompt templates, deterministic fixture backend for tests, configurable but disabled-by-default external agent backend, schema-normalization and validation of backend output, harness-owned executable evidence, evidence retention, lane failure isolation, preflight backend selection, manifest backend recording, cutover-readiness validation, semantic preflight fixtures, and updates to tests and documentation that describe the new behavior.

The child implementation areas under LIV-151 are explicitly:

- shared harness, backend selection, artifact retention, external backend safety contract, and cutover-readiness manifest support;
- correctness lane, concrete inverted authorization or control-condition fixture, and executable-evidence policy;
- test-quality lane;
- idioms and maintainability lane;
- security and performance lane;
- LIV-115 cutover readiness integration and default-backend switch.

Out of scope are posting PR comments, updating Linear from review commands, pushing branches, changing remote repository state, performing the LIV-115 cutover itself, or relying on a human reviewer as the final semantic review step. Creating the child Linear issues is in scope for this planning issue; remote mutation by review commands is not. The implementation may read a PR diff when the existing `--pr` mode is used, but it must not perform remote mutations and must not pass mutation credentials to lane agents.

The external backend must not be enabled by default by this plan. The shared harness child issue defines the command contract and safety checks so real runs can be tried manually, but LIV-115 decides when and whether external mode becomes the staged-review default after readiness passes.

The existing artifact filenames should remain stable unless a child issue deliberately updates the schema and all validation. The current important outputs are `review-brief.v1.json`, `review-lane-<lane>.v1.json`, `review-lane-<lane>.log`, `review-lane-<lane>-analysis.v1.json`, `review-synthesis.v1.json`, `final-review.v1.json`, `manifest.v1.json` or `preflight-manifest.v1.json`, and the new harness-owned `evidence-ledger.v1.json`.

## Milestones

The preparation milestone creates the implementation breakdown in Linear. At the end of this milestone, the child issues under LIV-151 exist with titles, descriptions, and acceptance criteria that match this plan. This milestone comes first because it makes the staged implementation executable by separate workers and prevents the review findings from being lost between issues.

The first implementation milestone is the shared harness child issue. At the end of this milestone, `scripts/scherzo-review run-lane` can assemble a lane input bundle, run a deterministic fixture backend, retain raw output and transcripts, normalize the result, validate it against the schema, and write a failed lane result when the backend fails. `scripts/scherzo-review preflight --agent-backend fixture` propagates the backend to every internal lane run, records the backend per lane in the manifest, and can be validated with `--require-cutover-ready`. The existing deterministic lane behavior remains available as a fallback until every specialist lane has been migrated. This milestone comes first because all later semantic lanes depend on the same safety and artifact boundary.

The second milestone is the correctness child issue. At the end of this milestone, the correctness lane is agent-backed through the shared harness and has a hard executable-evidence gate. It must detect the concrete `inverted-auth-control-condition` fixture and emit a blocking correctness finding only when a retained harness-run reproduction proves the behavior. Static-only concerns must be review notes. This milestone comes before other semantic lanes because the known cutover-blocking risk is a semantic correctness bug.

The third milestone is the test-quality child issue. At the end of this milestone, the test-quality lane inspects source changes, changed tests, nearby existing tests, and validation status to decide whether coverage is meaningful. It emits concrete testing findings for verified weak or missing behavioral coverage and emits coverage notes for non-blocking suspicions. This milestone follows correctness because correctness evidence often depends on tests and reproductions.

The fourth milestone is the idioms and maintainability child issue. At the end of this milestone, the idioms lane is agent-backed and distinguishes must-fix production safety problems from reviewability notes and optional nits. It should still honor the repository's lint policy for production `src/` code, including avoiding new `let assert`, `panic`, and `todo` unless explicitly justified through existing lint mechanisms.

The fifth milestone is the security and performance child issue. At the end of this milestone, the security/performance lane inspects boundary-sensitive changes, authorization and credential handling, command execution, filesystem/network parsing, daemon/process behavior, and growth or sleep-sensitive runtime changes. It emits concrete security or performance findings only when the evidence is specific enough and otherwise records risk notes.

The final milestone is cutover readiness for LIV-115. At the end of this milestone, the repository names the staged-review backend configuration surface that LIV-115 will switch, the safe default remains deterministic until readiness passes, `scripts/scherzo-review preflight --agent-backend fixture` includes semantic adversarial fixtures, all generated artifacts validate, lane failures are surfaced in synthesis, the preflight manifest reports `cutover_readiness.ready: true`, and final review artifacts preserve `remote_mutations: none`. LIV-115 may proceed only after this validation passes in the repository and the child issues are complete.

## Plan of Work

Start by creating the child issues because this plan intentionally delegates implementation to staged LIV-151 work. Use repository-relative description files under `tmp/linear-liv-152-child-issues/` and the repo-local Linear CLI. Each child issue description must include the relevant milestone goal, files to touch, tests to add, validation commands, and the acceptance criteria copied from this plan. If the CLI returns the new issue identifier, record it in this ExecPlan's Progress section when implementation begins.

For the shared harness, add an importable Python module under `scripts/scherzo_review/` rather than growing all agent behavior directly inside `scripts/scherzo-review`. Keep `scripts/scherzo-review` as the executable entrypoint. The entrypoint should continue to own argument parsing and should call the new harness functions from `run_lane_command`, `preflight_command`, and `validate_command`.

Add a backend selection surface to both `run-lane` and `preflight`. The accepted values are `heuristic`, `fixture`, and `external`. `heuristic` preserves the current deterministic path and remains the default until the readiness gate and LIV-115 integration say otherwise. `fixture` exercises the new agent harness deterministically and is the backend required for automated acceptance. `external` uses the command contract described below and fails with a schema-valid lane artifact when required configuration is missing.

Define a lane input bundle builder. It should take the lane id, brief path, loaded brief, diff text, source metadata, parsed `DiffFile` objects, output directory, lane configuration, selected backend, and scenario metadata. It should write a bundle under the lane output directory with repository-relative artifact paths. The bundle should include the consumed brief, `diff.patch`, `source-metadata.v1.json`, `changed-files.v1.json`, `validation-status.v1.json`, `context-manifest.v1.json`, collected changed-file snapshots under `context/`, and the lane prompt under `prompt.md`. The bundle should record SHA-256 hashes for every file so that `ReviewLaneResult.artifacts` and `evidence-ledger.v1.json` can refer to immutable evidence.

Changed-file context should be minimal and useful. For each changed file from the parsed diff, the harness should record path, previous path, change kind, language, subsystem, additions, deletions, hunks, hunk headers, and whether a current working-tree file exists at that path. For text files that exist in the current checkout, copy a bounded snapshot into `context/`. For deleted files or files absent from the checkout, record that the diff is the only available context. For very large files, record a truncated snapshot and include a truncation note in the context manifest. Do not write absolute local paths into artifacts.

The lane prompt should tell the agent that `ReviewBrief` is only orientation and cannot replace direct diff inspection. It should require the agent to inspect `diff.patch`, use changed-file context, read relevant repository files when available, consider validation status, and cite repository-relative locations. It should explicitly forbid remote mutation commands, PR comment posting, Linear updates, pushes, credential exfiltration, and edits to the working tree, but the plan does not rely on the prompt as the only control. It should require output as JSON that can be normalized into `ReviewLaneResult`.

The fixture backend consumes the same prompt and input bundle as a real backend and emits canned raw output based on scenario ids or fixture metadata. Fixture raw output must use the smaller lane-agent response shape, not a prebuilt final `ReviewLaneResult`, so the normalizer, evidence policy, path validation, and artifact retention are always exercised. The fixture backend must not require model access, network access, GitHub credentials, or Linear credentials.

The external backend contract is implemented in the shared harness but disabled unless explicitly requested with `--agent-backend external`. The command template is read from `SCHERZO_REVIEW_AGENT_COMMAND`; the timeout is read from `SCHERZO_REVIEW_AGENT_TIMEOUT_SECONDS` and defaults to 120 seconds when unset or invalid. The command template may contain these placeholders: `{lane_id}`, `{prompt_path}`, `{bundle_dir}`, `{output_dir}`, and `{raw_output_path}`. The harness expands placeholders only to repository-relative or lane-output paths, runs the command from the repository root, and requires the backend to write JSON to `{raw_output_path}`. Stdout and stderr are always retained as transcript artifacts. A missing command, timeout, nonzero exit code, missing raw output file, malformed JSON, schema-invalid output after normalization, dirty post-run working tree, or retained artifact outside the lane directory becomes a schema-valid failed lane result.

External backend execution must enforce containment rather than trusting the prompt. Build a sanitized environment by starting from a small allowlist such as `PATH`, `HOME`, `LANG`, `LC_ALL`, and any variables the child issue explicitly documents as read-only. Always remove `GITHUB_TOKEN`, `GH_TOKEN`, `LINEAR_API_KEY`, `SCHERZO_AGENT_LINEAR_API_KEY`, `SSH_AUTH_SOCK`, and similarly mutation-capable credentials. Capture working-tree state before and after execution. Reject absolute retained artifact paths and any path that escapes the lane output directory. If true OS-level sandboxing is not available, record that residual risk in the lane log and keep external mode out of the default path.

Normalize raw agent output before writing a lane result. The normalizer should accept either a complete `ReviewLaneResult` from trusted internal fixture code or a smaller lane-agent response containing findings, review notes, evidence requests, evidence references, and analysis summary. External raw output should use the smaller response shape. The normalizer should write `raw-agent-output.json`, stdout/stderr transcript artifacts, and any backend-provided diagnostic artifacts before normalization. It should fill standard lane metadata from `SPECIALIST_LANES`, set producer metadata, set input brief reference, add source metadata with diff hash, attach evidence artifact references, and validate with the existing artifact validator.

Implement harness-owned evidence before the correctness lane can produce blockers. Define an evidence request shape with `evidence_key`, `evidence_type`, `command`, `cwd`, `expected_exit_code`, `description`, and optional `timeout_seconds`. In the first implementation, the harness only runs commands that come from trusted scenario metadata or a future explicit allowlist; arbitrary commands suggested by an external agent are recorded as requests but are not executed. When the harness runs an allowed evidence command, it writes command, stdout, stderr, exit code, timeout status, working-tree state, and hashes into `evidence-ledger.v1.json`. A correctness finding that claims `blocking: true` must reference one of those ledger evidence ids. If the ledger entry is missing, has the wrong evidence type, timed out, has an unexpected exit code, or dirtied the tree, the finding is downgraded to a review note and the harness records `downgraded_unverified_correctness_claim`.

For lane failures, keep the existing `write_failed_lane_artifacts` behavior but extend it to reference the input bundle, prompt, raw output if any, transcript if any, evidence ledger if any, backend metadata, and error log. A malformed raw response, timeout, missing backend command, invalid artifact, working-tree mutation, or prompt contract violation should produce a failed lane result rather than crashing without artifacts. Synthesis should continue to record lane execution issues and lane failure counts.

For preflight, extend `preflight_scenarios()` with semantic fixtures and backend metadata. `preflight --agent-backend fixture` must pass the selected backend into every internal lane run. The preflight manifest must record the selected backend at the top level and for each lane run, with fields such as `agent_backend`, `lane_runs[].lane_id`, `lane_runs[].backend`, `lane_runs[].execution_status`, and `lane_runs[].artifact_path`. The manifest must also include `cutover_readiness` with `ready`, `required_backend`, `required_lanes`, and `blocking_reasons`. Negative scenarios that intentionally simulate lane failures should be marked in scenario metadata so they validate failure containment without making the all-up readiness signal ambiguous.

Add `scripts/scherzo-review validate --require-cutover-ready` for preflight manifests. When this flag is present, validation succeeds only if the artifact is a preflight manifest, the backend is `fixture` or `external`, all required non-negative readiness scenarios ran, all required lanes succeeded in those scenarios, required semantic fixture assertions passed, and `remote_mutations` is `none`. It prints `REVIEW_CUTOVER_READY=ok` on success and a clear nonzero failure with `REVIEW_CUTOVER_READY=failed` and one blocking reason on failure.

The most important new semantic fixture is `inverted-auth-control-condition`. It should create a local diff for `src/liv_152_fixture/project_authorization.gleam`. The before snippet is:

    pub type Role {
      Admin
      User
    }

    pub fn delete_project(role: Role) -> Result(String, String) {
      case role {
        Admin -> Ok("deleted")
        User -> Error("forbidden")
      }
    }

The after snippet intentionally inverts the unauthorized branch:

    pub type Role {
      Admin
      User
    }

    pub fn delete_project(role: Role) -> Result(String, String) {
      case role {
        Admin -> Ok("deleted")
        User -> Ok("deleted")
      }
    }

The scenario should write `repro/inverted_auth_control_condition_repro.py` under the scenario's correctness lane output directory. The reproduction command is `python3 repro/inverted_auth_control_condition_repro.py`, run with the lane output directory as `cwd`. The script returns exit code 0 only when it observes the bug and prints exactly `REPRODUCED: unauthorized User received Ok("deleted")`. If the bug is absent, it exits 1 and prints `NOT_REPRODUCED: unauthorized User was rejected`. The correctness lane result for this scenario must include one blocking correctness finding with `evidence_type: reproduction`, `verified: true`, a location pointing at `src/liv_152_fixture/project_authorization.gleam`, and an evidence id from `evidence-ledger.v1.json` that references the reproduction command log, stdout, and stderr artifacts. The final review should have one blocker and `remote_mutations: none`.

Add a companion fixture named `auth-control-static-suspicion-without-repro`. It should create a diff for `src/liv_152_fixture/workflow_gate.gleam` that changes a guard in a way that is suspicious from static reading but has no trusted harness reproduction command. The fixture backend should emit a correctness concern without a valid evidence id. The expected output is no blocking correctness finding. The correctness lane should emit a `review_notes` item with `kind: risk_note` or `follow_up_test`, category `correctness`, and a suggested action to add executable coverage. This fixture proves the distinction between suspicions and findings.

## Concrete Steps

Use these steps when implementing the future child issues. Each step should be performed from the repository root. Commit after each milestone when the listed validation for that milestone passes.

For child issue creation:

1. Create the description directory:

       mkdir -p tmp/linear-liv-152-child-issues

2. Write `tmp/linear-liv-152-child-issues/shared-agent-lane-harness.md`. Its description should say that it implements `scripts/scherzo_review/agent_lane_harness.py`, `run-lane --agent-backend`, `preflight --agent-backend`, backend manifest recording, external backend containment, fixture backend raw output, failed lane artifacts, `evidence-ledger.v1.json`, and `validate --require-cutover-ready`. Its acceptance criteria are the shared-harness validation commands in this section.
3. Create the shared harness child issue:

       direnv exec . linear issue create --team LIV --parent LIV-152 --title "Implement staged review agent-lane harness" --description-file tmp/linear-liv-152-child-issues/shared-agent-lane-harness.md --no-interactive

   The expected output names a new `LIV-<number>` issue. Record that identifier in this plan's Progress section when implementation begins.
4. Write `tmp/linear-liv-152-child-issues/correctness-agent-lane.md`. Its description should name the `inverted-auth-control-condition` and `auth-control-static-suspicion-without-repro` fixtures, the harness-owned evidence requirement, and the correctness validation commands below.
5. Create the correctness child issue:

       direnv exec . linear issue create --team LIV --parent LIV-152 --title "Implement agent-backed correctness lane evidence gate" --description-file tmp/linear-liv-152-child-issues/correctness-agent-lane.md --no-interactive

6. Write and create the test-quality child issue with title `Implement agent-backed test quality lane` and description file `tmp/linear-liv-152-child-issues/test-quality-agent-lane.md`. The description should require fixtures for missing meaningful coverage, helper-only test churn, assertion-bearing tests that cover changed behavior, and tests that exist but miss the changed branch.
7. Write and create the idioms child issue with title `Implement agent-backed maintainability lane` and description file `tmp/linear-liv-152-child-issues/idioms-maintainability-agent-lane.md`. The description should require production lint policy preservation, review notes for broad reviewability observations, and non-blocking treatment for low-value nits.
8. Write and create the security/performance child issue with title `Implement agent-backed security and performance lane` and description file `tmp/linear-liv-152-child-issues/security-performance-agent-lane.md`. The description should require fixtures for hard-coded credentials, shell hazards, authorization-sensitive changes, performance-sensitive sleeps or growth, and non-blocking boundary risk notes.
9. Write and create the cutover readiness child issue with title `Wire staged review cutover readiness for LIV-115` and description file `tmp/linear-liv-152-child-issues/cutover-readiness.md`. The description should require locating or adding the backend configuration surface consumed by LIV-115, proving its default remains safe, and proving it refuses cutover when `validate --require-cutover-ready` fails.
10. Commit only the plan updates or issue-description helper files if they are intentionally retained. Do not commit generated Linear CLI output.

For the shared harness child issue:

1. Add a failing test in `test/review_artifacts_test.gleam` named `agent_fixture_lane_writes_bundle_artifacts`. Use the existing script-running helpers in that file. The test should create a tiny local diff, run `scripts/scherzo-review dry-run`, then run `scripts/scherzo-review run-lane --lane correctness --agent-backend fixture`, and assert that the command eventually writes `review-lane-correctness.v1.json`, `input/diff.patch`, `input/changed-files.v1.json`, `prompt.md`, `raw-agent-output.json`, and `review-lane-correctness.log`.
2. Run `direnv exec . gleam test` and confirm the new test fails because `run-lane` does not recognize `--agent-backend` or does not write the fixture artifacts yet.
3. Create `scripts/scherzo_review/__init__.py` as an empty package marker.
4. Create `scripts/scherzo_review/agent_lane_harness.py` with `AGENT_BACKENDS = {"heuristic", "fixture", "external"}`, `parse_agent_backend(value)`, `sha256_file(path)`, `repo_relative_path(path)`, and `validate_retained_artifact_path(output_dir, path)`.
5. Run `direnv exec . python3 -m py_compile scripts/scherzo-review scripts/scherzo_review/agent_lane_harness.py` and expect no syntax errors.
6. In `scripts/scherzo-review`, import the harness module and add `--agent-backend` to the `run-lane` parser with choices `heuristic`, `fixture`, and `external`, defaulting to `heuristic`.
7. In `run_lane_command`, parse the selected backend. When it is `heuristic`, preserve the current deterministic `run_specialist_lane` path with no behavior change. When it is `fixture` or `external`, call a temporary harness stub that raises `ReviewError("agent backend not implemented")` so the failing test now reaches the intended path.
8. Run `direnv exec . gleam test` and confirm the new test now fails with the temporary harness error, while existing heuristic-path tests still pass or fail only because the full suite stops at the new red test.
9. Add `build_lane_input_bundle(lane_id, brief_path, brief, diff, source, files, output_dir, prompt_path, backend, scenario_id)` to `scripts/scherzo_review/agent_lane_harness.py`. It must write `input/review-brief.v1.json`, `input/diff.patch`, `input/source-metadata.v1.json`, `input/changed-files.v1.json`, `input/validation-status.v1.json`, `input/context-manifest.v1.json`, copied context files when present, and `prompt.md`.
10. Extend the red Gleam test to parse `input/changed-files.v1.json` and assert that every path is repository-relative and that the changed file from the tiny diff is present.
11. Implement `run_fixture_agent(bundle, scenario_id, output_dir, raw_output_path)` so the tiny default fixture writes a smaller lane-agent response with no findings, one harmless review note, and an analysis summary. It must write JSON to `raw-agent-output.json` instead of directly writing `ReviewLaneResult`.
12. Add `normalize_agent_response(lane_id, brief_path, brief, source, diff, raw_output, bundle, output_dir)` and wire it to create a schema-valid `ReviewLaneResult` with lane metadata from `SPECIALIST_LANES`, input brief reference, source metadata, artifacts for the bundle, prompt, raw output, and log.
13. Run `direnv exec . gleam test` and confirm `agent_fixture_lane_writes_bundle_artifacts` passes through the fixture path and existing tests still pass.
14. Add a failing test in `test/review_artifacts_test.gleam` named `preflight_fixture_backend_records_lane_backends`. It should run `scripts/scherzo-review preflight --agent-backend fixture --scenario no-meaningful-findings --output-dir tmp/scherzo-review-preflight-fixture-test`, parse `preflight-manifest.v1.json`, and assert top-level `agent_backend: fixture` and at least one lane run with `backend: fixture`.
15. Run `direnv exec . gleam test` and confirm the new preflight test fails because `preflight` does not accept or propagate `--agent-backend` yet.
16. Add `--agent-backend heuristic|fixture|external` to the `preflight` parser, defaulting to `heuristic`.
17. Update `preflight_command` to pass the selected backend into every internal `run-lane` invocation and to write `agent_backend` plus `lane_runs[].backend` in `preflight-manifest.v1.json`.
18. Run `direnv exec . gleam test` and confirm the preflight backend recording test passes.
19. Add `sanitize_agent_environment(env)`, `capture_repo_state(repo_root)`, and `run_external_agent(bundle, command_template, timeout_seconds, output_dir, raw_output_path)` to the harness. The first implementation should fail external mode with a schema-valid lane result when `SCHERZO_REVIEW_AGENT_COMMAND` is missing.
20. Add a Gleam test that runs `run-lane --agent-backend external` without `SCHERZO_REVIEW_AGENT_COMMAND`, expects the command to exit through the failed-lane path, validates `review-lane-correctness.v1.json`, and asserts `execution_status.state: failed` and an error summary mentioning missing external backend configuration.
21. Add tests or Python-level assertions that `sanitize_agent_environment` removes `GITHUB_TOKEN`, `GH_TOKEN`, `LINEAR_API_KEY`, `SCHERZO_AGENT_LINEAR_API_KEY`, and `SSH_AUTH_SOCK` from the environment passed to the external command.
22. Implement external command execution with `SCHERZO_REVIEW_AGENT_COMMAND`, placeholder expansion, `SCHERZO_REVIEW_AGENT_TIMEOUT_SECONDS`, stdout and stderr transcript retention, path containment, post-run working-tree state capture, and conversion of timeout, nonzero exit, missing raw output, malformed JSON, or dirty tree into failed lane artifacts.
23. Add `run_evidence_command(evidence_request, lane_output_dir, timeout_seconds)`, `write_evidence_ledger(output_dir, entries)`, and `load_evidence_ledger(output_dir)` to the harness.
24. Add `enforce_correctness_evidence_policy(lane_result, evidence_ledger, output_dir)`. It must downgrade any blocking correctness finding that lacks a valid harness evidence id, executable evidence type, expected exit status, and clean working-tree state.
25. Add a red/green test that feeds the fixture backend a blocking correctness finding without a valid evidence id and asserts the final lane result has no blocking correctness finding and has a `review_notes` entry with `harness_actions` including `downgraded_unverified_correctness_claim`.
26. Extend `write_failed_lane_artifacts` in `scripts/scherzo-review` so it can include input bundle, prompt, raw output, transcript, evidence ledger, backend metadata, and error log artifacts when those files exist.
27. Add `evaluate_cutover_readiness(preflight_manifest, required_backend)` and `validate_cutover_readiness(preflight_manifest)` to the harness or `scripts/scherzo-review`. Required lanes are `correctness`, `test-quality`, `idioms-maintainability`, and `security-performance`; required backend is `fixture` for automated acceptance.
28. Update `preflight_command` to write `cutover_readiness` with `ready`, `required_backend`, `required_lanes`, and `blocking_reasons`. A heuristic run must set `ready: false` with a reason mentioning the backend.
29. Add `--require-cutover-ready` to `validate_command`. It must accept only preflight manifests with `cutover_readiness.ready: true`; otherwise it exits nonzero and prints `REVIEW_CUTOVER_READY=failed` with the first blocking reason.
30. Run the shared-harness validation commands:

       direnv exec . python3 -m py_compile scripts/scherzo-review scripts/scherzo_review/agent_lane_harness.py
       direnv exec . gleam test
       direnv exec . scripts/scherzo-review preflight --agent-backend fixture --output-dir tmp/scherzo-review-preflight
       direnv exec . scripts/scherzo-review validate --artifact tmp/scherzo-review-preflight/preflight-manifest.v1.json

31. Commit the shared harness with a message such as `Add staged review agent lane harness`.

For the correctness child issue:

1. Update `scripts/scherzo_review/prompts/correctness.md` so it requires direct diff inspection, nearby source inspection, validation status review, and harness-issued executable evidence ids for blockers.
2. Add the `inverted-auth-control-condition` scenario to `preflight_scenarios()` with path `src/liv_152_fixture/project_authorization.gleam`, the before and after snippets from the Plan of Work section, scenario metadata `agent_fixture_id: inverted-auth-control-condition`, and one trusted evidence request keyed `inverted_auth_repro`.
3. Have the scenario generate `repro/inverted_auth_control_condition_repro.py` under the correctness lane output directory before the fixture backend response is normalized. The script must print `REPRODUCED: unauthorized User received Ok("deleted")` and exit 0 when the after-version behavior allows `User` to delete.
4. Add fixture backend raw output for `inverted-auth-control-condition`. It should propose one high-severity blocking correctness finding at `src/liv_152_fixture/project_authorization.gleam`, reference evidence key `inverted_auth_repro`, and not directly fabricate a final `ReviewLaneResult`.
5. Run `direnv exec . scripts/scherzo-review preflight --agent-backend fixture --scenario inverted-auth-control-condition --output-dir tmp/scherzo-review-preflight-correctness-red` before evidence enforcement is fully wired and confirm it fails the scenario-specific assertion or cutover readiness check.
6. Wire the scenario evidence request into `run_evidence_command`, write `evidence-ledger.v1.json`, and map the fixture finding's evidence key to the harness-issued evidence id.
7. Add assertions in `preflight_assert_scenario_expectations()` for `inverted-auth-control-condition`: final blocker count is at least one, the correctness lane finding is verified, evidence type is `reproduction`, evidence id exists in the ledger, command is `python3 repro/inverted_auth_control_condition_repro.py`, exit code is 0, stdout contains `REPRODUCED: unauthorized User received Ok("deleted")`, the first location path is `src/liv_152_fixture/project_authorization.gleam`, and `remote_mutations` remains `none`.
8. Add the `auth-control-static-suspicion-without-repro` scenario for `src/liv_152_fixture/workflow_gate.gleam`. Do not provide a trusted evidence request. The fixture backend should emit a correctness concern that looks plausible but lacks a valid evidence id.
9. Add assertions that the static-suspicion scenario has no blocking correctness finding, has a correctness `review_notes` item with `kind: risk_note` or `follow_up_test`, and records a suggested action to add executable coverage.
10. Add a malformed correctness output fixture case and assert that it writes a failed lane artifact, not a crash without artifacts.
11. Add or update Gleam tests in `test/review_artifacts_test.gleam` to run only the two correctness scenarios with repeated `--scenario` arguments and inspect the generated lane result JSON plus `evidence-ledger.v1.json`.
12. Run the correctness validation commands:

       direnv exec . gleam test
       direnv exec . scripts/scherzo-review preflight --agent-backend fixture --scenario inverted-auth-control-condition --scenario auth-control-static-suspicion-without-repro --output-dir tmp/scherzo-review-preflight-correctness
       direnv exec . scripts/scherzo-review validate --artifact tmp/scherzo-review-preflight-correctness/inverted-auth-control-condition/02-lane-correctness/review-lane-correctness.v1.json
       direnv exec . scripts/scherzo-review validate --artifact tmp/scherzo-review-preflight-correctness/inverted-auth-control-condition/03-synthesis/final-review.v1.json
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

13. Commit the correctness lane with a message such as `Add agent-backed correctness lane evidence gate`.

For the test-quality child issue:

1. Update `scripts/scherzo_review/prompts/test_quality.md` so it asks the agent to compare implementation changes, changed tests, existing nearby tests, and supplied validation status.
2. Add fixture backend cases for missing tests on a semantic behavior change, helper-only test churn, assertion-bearing tests that cover the changed behavior, and tests that exist but do not exercise the changed branch.
3. Normalize non-blocking coverage concerns as `review_notes` with `kind: coverage_note` unless the lane can cite concrete evidence that a test is misleading or missing for changed behavior.
4. Add preflight scenarios for semantic implementation changes with no meaningful coverage and for a well-tested change that should produce no testing finding.
5. Update `test/review_artifacts_test.gleam` to assert that coverage notes and testing findings are distinct and schema-valid.
6. Run:

       direnv exec . gleam test
       direnv exec . scripts/scherzo-review preflight --agent-backend fixture --output-dir tmp/scherzo-review-preflight-test-quality
       direnv exec . scripts/scherzo-review validate --artifact tmp/scherzo-review-preflight-test-quality/preflight-manifest.v1.json
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

7. Commit the test-quality lane with a message such as `Add agent-backed test quality lane`.

For the idioms and maintainability child issue:

1. Update `scripts/scherzo_review/prompts/idioms_maintainability.md` so it asks the agent to inspect production source, changed public interfaces, error handling, module organization, and reviewability.
2. Preserve the repository production lint policy as a must-fix source of findings for `src/`: new production `let assert`, `panic`, or `todo` should remain blocking maintainability findings unless there is a narrow documented suppression that the lint policy accepts.
3. Add fixture backend cases for a real maintainability blocker, a large but well-structured diff that should only create a review note, and a low-value nit that should not block.
4. Add preflight assertions that maintainability review notes are retained separately from findings.
5. Run:

       direnv exec . gleam format --check src test
       direnv exec . gleam test
       direnv exec . scripts/scherzo-review preflight --agent-backend fixture --output-dir tmp/scherzo-review-preflight-idioms
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

6. Commit the idioms lane with a message such as `Add agent-backed maintainability lane`.

For the security and performance child issue:

1. Update `scripts/scherzo_review/prompts/security_performance.md` so it asks the agent to inspect authorization, secrets, command execution, filesystem and network boundaries, parsing, daemon/process behavior, sleeps, polling, unbounded loops, retained data, and hot paths.
2. Add fixture backend cases for a concrete hard-coded credential, a concrete command injection or shell hazard, an authorization-sensitive change that should coordinate with correctness output, a performance-sensitive sleep or unbounded growth note, and a boundary-sensitive change that should remain a risk note.
3. Ensure security findings require specific evidence and locations. Broad boundary sensitivity without a specific vulnerability remains `review_notes` with `kind: risk_note`.
4. Add preflight assertions that security/performance notes prioritize non-documentation locations when both docs and runtime files are changed.
5. Run:

       direnv exec . gleam test
       direnv exec . scripts/scherzo-review preflight --agent-backend fixture --output-dir tmp/scherzo-review-preflight-security-performance
       direnv exec . scripts/scherzo-review validate --artifact tmp/scherzo-review-preflight-security-performance/preflight-manifest.v1.json
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

6. Commit the security/performance lane with a message such as `Add agent-backed security performance lane`.

For LIV-115 cutover readiness:

1. Locate the current staged-review integration point for LIV-115 by searching for calls to `scripts/scherzo-review`, staged review workflow config, or review-lane defaults. Record the exact file and setting in this plan's Decision Log before changing behavior.
2. If no integration point exists, add a small documented config surface for the staged review backend with a safe default of `heuristic`. Do not change unrelated command behavior.
3. Add a test proving the default remains deterministic until `validate --require-cutover-ready` passes.
4. Add a negative readiness test: a simulated failed correctness lane in a non-negative readiness scenario must make `validate --require-cutover-ready` exit nonzero.
5. Change the default backend for staged review lanes only after all child issue validations pass. If a feature flag or workflow config exists by then, switch that config rather than changing unrelated command behavior.
6. Run full preflight with the fixture backend and, where safe and configured, a manual external-agent dry run against a local diff. Do not post comments or update Linear from review commands.
7. Validate representative artifacts from the full preflight:

       direnv exec . scripts/scherzo-review preflight --agent-backend fixture --output-dir tmp/scherzo-review-preflight
       direnv exec . scripts/scherzo-review validate --artifact tmp/scherzo-review-preflight/preflight-manifest.v1.json --require-cutover-ready
       direnv exec . scripts/scherzo-review validate --artifact tmp/scherzo-review-preflight/inverted-auth-control-condition/02-lane-correctness/review-lane-correctness.v1.json
       direnv exec . scripts/scherzo-review validate --artifact tmp/scherzo-review-preflight/inverted-auth-control-condition/03-synthesis/review-synthesis.v1.json
       direnv exec . scripts/scherzo-review validate --artifact tmp/scherzo-review-preflight/inverted-auth-control-condition/03-synthesis/final-review.v1.json
       direnv exec . gleam format --check src test
       direnv exec . gleam test
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

8. Confirm every generated final review artifact contains `remote_mutations` equal to `none`.
9. Update LIV-115 only after the above commands pass and the child issues are complete.

## Testing and Falsifiability

The shared harness tests should prove that the harness writes all required local artifacts without relying on a live model. A new test in `test/review_artifacts_test.gleam` should create a small diff, run `scripts/scherzo-review dry-run`, then run `scripts/scherzo-review run-lane --lane correctness --agent-backend fixture`, and assert that `review-lane-correctness.v1.json` validates, references the input brief, includes bundle or prompt artifacts, includes `raw-agent-output.json`, includes a log, and preserves repository-relative paths.

Preflight backend tests should prove that validation cannot accidentally pass through the old heuristic path. A test should run `scripts/scherzo-review preflight --agent-backend fixture --scenario no-meaningful-findings --output-dir tmp/scherzo-review-preflight-fixture-test` and assert that `preflight-manifest.v1.json` includes `agent_backend: fixture`, every lane run includes `backend: fixture`, and final artifacts still show `remote_mutations: none`. A separate test should run a heuristic preflight and assert that `validate --require-cutover-ready` fails with a backend-related blocking reason.

Harness-owned evidence tests should prove the central safety claim independently of any prompt. A fixture response that emits a blocking correctness finding without a valid evidence id should be downgraded into a correctness review note. A fixture response that references an evidence id whose command timed out, exited with the wrong code, or dirtied the working tree should also be downgraded. Only a finding that references a ledger entry with `evidence_type: reproduction`, the expected exit code, retained stdout and stderr artifacts, hashes, and clean pre-run and post-run state may remain blocking.

The correctness tests should prove the central semantic claim. The `inverted-auth-control-condition` fixture should fail before the correctness implementation because no semantic blocking finding with harness-owned evidence is produced. After implementation, the fixture should pass by producing a correctness lane result with exactly the expected shape: `execution_status.state` is `succeeded`; at least one finding has `category: correctness`, `severity: high` or `critical`, `blocking: true`, `verified: true`, and `evidence_type: reproduction`; the first location path is `src/liv_152_fixture/project_authorization.gleam`; the finding references a harness evidence id; `evidence-ledger.v1.json` records `python3 repro/inverted_auth_control_condition_repro.py`, exit code 0, clean working-tree state, and stdout containing `REPRODUCED: unauthorized User received Ok("deleted")`; and the synthesized final review has a positive blocking count. If this fixture does not produce a blocker, the plan's core claim is false.

The static-suspicion correctness fixture should falsify overblocking. It should contain a suspicious auth or control-flow change without executable proof. After implementation, the correctness lane must not emit a blocking correctness finding. It should emit a `review_notes` item with a follow-up action. If the lane blocks on static suspicion alone, the implementation violates this plan.

Test-quality fixtures should distinguish missing or weak meaningful coverage from non-blocking coverage notes. A source-only behavior change with no tests should produce a testing finding or a high-priority coverage output as specified by the child issue. Helper-only test churn should produce a coverage note rather than a misleading weak-test finding. A behavior change with focused assertion-bearing tests should produce no meaningful testing finding. If these scenarios collapse into generic findings, the implementation is not semantic enough.

Idioms and maintainability fixtures should verify that production safety policy remains enforceable and that reviewability context remains non-blocking. A production `src/` diff adding `panic`, `todo`, or `let assert` should be a must-fix maintainability finding. A large but coherent diff should be a review note unless the lane cites a concrete maintainability problem. Optional whitespace or wording nits must not become blockers.

Security and performance fixtures should verify that concrete vulnerabilities are findings and broad sensitivity is a note. A hard-coded credential should be a blocking security finding. A concrete shell execution hazard should be a security finding. Filesystem, parsing, process, sleep, or growth-sensitive changes without a proven exploit or bug should be retained as risk notes. If the lane turns broad sensitivity into blockers, it is too noisy for cutover.

External backend safety tests should prove containment. Missing `SCHERZO_REVIEW_AGENT_COMMAND`, timeout, nonzero exit, missing raw output, malformed JSON, schema-invalid normalized output, retained artifact path escape, and dirty post-run working tree should each write a schema-valid failed `ReviewLaneResult`, retain diagnostic logs, and allow synthesis to record an execution issue. Environment tests should prove mutation-capable credentials are not passed to the external process.

Failure-path tests should prove containment. A simulated timeout, missing external command, malformed JSON response, and schema-invalid lane result should each write a schema-valid failed `ReviewLaneResult`, retain diagnostic logs, and allow synthesis to record an execution issue. If one failed lane prevents other lane artifacts from being written or hides the failure in the final review, the implementation is unsafe.

Dry-run safety tests should inspect every preflight manifest and final review artifact and assert `remote_mutations` is `none`. The fixture preflight suite should not require GitHub, Linear, network access, or credentials. If a test requires remote mutation or fails without tokens, the implementation violates the workflow contract.

## Validation and Acceptance

The final implementation is accepted only when these commands pass from the repository root:

    direnv exec . scripts/scherzo-review preflight --agent-backend fixture --output-dir tmp/scherzo-review-preflight
    direnv exec . scripts/scherzo-review validate --artifact tmp/scherzo-review-preflight/preflight-manifest.v1.json --require-cutover-ready
    direnv exec . scripts/scherzo-review validate --artifact tmp/scherzo-review-preflight/inverted-auth-control-condition/02-lane-correctness/review-lane-correctness.v1.json
    direnv exec . scripts/scherzo-review validate --artifact tmp/scherzo-review-preflight/inverted-auth-control-condition/03-synthesis/review-synthesis.v1.json
    direnv exec . scripts/scherzo-review validate --artifact tmp/scherzo-review-preflight/inverted-auth-control-condition/03-synthesis/final-review.v1.json
    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

A successful fixture-backed preflight prints output shaped like:

    REVIEW_PREFLIGHT=ok
    REVIEW_SCHEMA_VERSION=1
    REVIEW_AGENT_BACKEND=fixture
    REVIEW_PREFLIGHT_ARTIFACT_DIR=tmp/scherzo-review-preflight
    REVIEW_PREFLIGHT_MANIFEST_PATH=tmp/scherzo-review-preflight/preflight-manifest.v1.json
    REVIEW_REMOTE_MUTATIONS=none

A successful cutover-ready manifest validation prints output shaped like:

    REVIEW_ARTIFACT_VALID=ok
    REVIEW_SCHEMA_VERSION=1
    REVIEW_ARTIFACT_TYPE=preflight_manifest
    REVIEW_CUTOVER_READY=ok

A successful targeted lane validation prints output shaped like:

    REVIEW_ARTIFACT_VALID=ok
    REVIEW_SCHEMA_VERSION=1
    REVIEW_ARTIFACT_TYPE=review_lane_result

The `inverted-auth-control-condition` final review artifact must show at least one blocker from the correctness lane and must preserve `remote_mutations: none`. Its correctness lane output must include `evidence-ledger.v1.json`, and the blocking finding must reference a harness evidence id whose ledger entry records the reproduction command, exit code 0, expected stdout, retained output artifacts, and clean working-tree state. The `auth-control-static-suspicion-without-repro` final review artifact must show no blocking correctness finding and must include a correctness review note. Lane failure fixtures must show lane failure counts and execution issues in synthesis without hiding other lane outputs.

The preflight manifest must record `agent_backend: fixture`, `lane_runs[].backend: fixture` for every agent-backed lane run, and `cutover_readiness.ready: true` with no blocking reasons. A manifest produced by the default heuristic backend is useful for backward compatibility but is not acceptable evidence for LIV-115 readiness.

A manual external-agent dry run may be performed only after `SCHERZO_REVIEW_AGENT_COMMAND` is configured for the operator environment. That manual run is supplemental evidence and must still produce local artifacts only, preserve `remote_mutations: none`, avoid dirtying the working tree, and pass normal artifact validation. The exact production command and provider remain a `[CLARIFY]` item until stakeholders choose them.

The plan is not accepted if the implementation requires a human reviewer as the final semantic step, mutates GitHub or Linear from review commands, writes absolute local paths into artifacts, accepts schema-invalid lane output, permits blocking correctness findings without harness-owned executable evidence, omits backend metadata from preflight, allows `validate --require-cutover-ready` to pass for the heuristic backend, or allows LIV-115 to cut over while any agent-backed lane child issue remains incomplete.

## Rollout, Recovery, and Idempotence

Roll out additively. Keep the existing deterministic lane path available while the shared harness and individual agent-backed lanes are introduced. The default backend should not switch to external agents until all child issue validations pass, fixture-backed preflight reports cutover readiness, and LIV-115 explicitly switches the staged-review integration point. If a child issue fails or produces noisy artifacts, leave the default on the previous safe backend and fix the child issue without affecting LIV-115.

The implementation is idempotent because review commands write to explicit output directories under `tmp/`. It is safe to delete a preflight output directory and rerun the command. Repeated fixture dry-runs should produce new timestamps and hashes but the same semantic pass/fail results for deterministic fixtures. Harness-owned evidence commands must write only inside the lane output directory so reruns do not leave repository changes behind.

Rollback is straightforward before LIV-115: configure staged review to use the previous deterministic backend or do not enable the agent-backed lane default. After LIV-115, rollback means disabling the cutover and returning to the previous non-staged review workflow until the agent lanes are fixed. The final fixture-backed preflight, cutover-ready manifest validation, and child issue completion must therefore remain hard prerequisites for LIV-115.

Contain failed lanes by preserving their artifacts and failing closed for cutover readiness. A failed lane should not post a partial review remotely. It should write a failed lane artifact, preserve logs, appear in synthesis, add a blocking readiness reason when it occurs in a required readiness scenario, and cause operators to rerun or fix the lane before trusting the review.

The agent environment should be clean. The harness should not pass mutation-capable tokens to agents. Agents should receive local diff and context files. If an external agent needs read-only repository inspection, it should operate from the repository root and write only inside the lane output directory. Any command that would mutate the working tree, push, comment, update Linear, or write retained artifacts outside the lane directory is outside the lane contract and must fail that lane.

The LIV-115 backend switch must be localized. The cutover readiness child issue must name the exact config field or workflow setting before changing it. If no such field exists, create one with a safe default and a test proving the default remains deterministic until `validate --require-cutover-ready` passes.

## Artifacts and Notes

A successful agent-backed lane output directory should look like this, with all paths repository-relative in artifacts:

    tmp/scherzo-review-preflight/inverted-auth-control-condition/02-lane-correctness/
      input/
        review-brief.v1.json
        diff.patch
        source-metadata.v1.json
        changed-files.v1.json
        validation-status.v1.json
        context-manifest.v1.json
        context/<safe-repository-path>.txt
      prompt.md
      transcript.stdout.txt
      transcript.stderr.txt
      raw-agent-output.json
      evidence-ledger.v1.json
      repro/
        inverted_auth_control_condition_repro.py
        inverted_auth_control_condition.command.log
        inverted_auth_control_condition.stdout.txt
        inverted_auth_control_condition.stderr.txt
      review-lane-correctness-analysis.v1.json
      review-lane-correctness.log
      review-lane-correctness.v1.json

The expected core of the inverted authorization fixture's correctness lane result is:

    artifact_type: review_lane_result
    lane.id: correctness
    execution_status.state: succeeded
    findings[0].category: correctness
    findings[0].blocking: true
    findings[0].verified: true
    findings[0].evidence_type: reproduction
    findings[0].evidence_id: matches an entry in evidence-ledger.v1.json
    findings[0].summary: mentions the inverted authorization or control condition
    findings[0].locations[0].path: src/liv_152_fixture/project_authorization.gleam
    artifacts: includes the reproduction command log, stdout, stderr, and evidence ledger

The expected core of the harness evidence ledger entry is:

    evidence_id: inverted-auth-control-condition/inverted_auth_repro
    evidence_type: reproduction
    command: python3 repro/inverted_auth_control_condition_repro.py
    cwd: tmp/scherzo-review-preflight/inverted-auth-control-condition/02-lane-correctness
    expected_exit_code: 0
    exit_code: 0
    timed_out: false
    stdout_artifact: repro/inverted_auth_control_condition.stdout.txt
    stderr_artifact: repro/inverted_auth_control_condition.stderr.txt
    stdout_contains: REPRODUCED: unauthorized User received Ok("deleted")
    working_tree_before: clean
    working_tree_after: clean

The expected core of the static suspicion fixture's correctness lane result is:

    artifact_type: review_lane_result
    lane.id: correctness
    execution_status.state: succeeded
    findings: does not contain a blocking correctness finding
    review_notes: contains a correctness risk_note or follow_up_test with a suggested executable check
    harness_actions: includes downgraded_unverified_correctness_claim when the fixture response asked for a blocker

The expected core of the preflight manifest after fixture-backed validation is:

    artifact_type: preflight_manifest
    agent_backend: fixture
    lane_runs[].lane_id: one of correctness, test-quality, idioms-maintainability, security-performance
    lane_runs[].backend: fixture
    cutover_readiness.ready: true
    cutover_readiness.required_backend: fixture
    cutover_readiness.blocking_reasons: []
    remote_mutations: none

Every artifact manifest, synthesis artifact, and final review artifact must continue to record `remote_mutations: none`.

## Interfaces and Dependencies

Do not add a Python dependency unless a child issue proves the standard library is insufficient. The existing script already uses Python standard library modules for argument parsing, JSON, hashing, subprocesses, paths, and timestamps.

The planned harness module should expose functions with these responsibilities. Exact Python names may change during implementation only if this plan is updated first; otherwise use these names so the child issues share one contract:

    AGENT_BACKENDS = {"heuristic", "fixture", "external"}
    parse_agent_backend(value: str) -> str
    sha256_file(path) -> str
    repo_relative_path(path) -> str
    validate_retained_artifact_path(output_dir, path) -> str
    build_lane_input_bundle(lane_id, brief_path, brief, diff, source, files, output_dir, prompt_path, backend, scenario_id=None) -> dict
    run_fixture_agent(bundle, scenario_id, output_dir, raw_output_path) -> dict
    sanitize_agent_environment(env: dict) -> dict
    capture_repo_state(repo_root) -> dict
    run_external_agent(bundle, command_template, timeout_seconds, output_dir, raw_output_path) -> dict
    normalize_agent_response(lane_id, brief_path, brief, source, diff, raw_output, bundle, output_dir) -> dict
    run_evidence_command(evidence_request, lane_output_dir, timeout_seconds) -> dict
    write_evidence_ledger(output_dir, entries) -> str
    load_evidence_ledger(output_dir) -> dict
    enforce_correctness_evidence_policy(lane_result, evidence_ledger, output_dir) -> dict
    enforce_lane_contract(lane_id, lane_result, evidence_ledger, output_dir) -> dict
    write_agent_failure_result(lane_id, brief_path, output_dir, message, bundle=None, backend=None) -> str
    evaluate_cutover_readiness(preflight_manifest, required_backend="fixture") -> dict
    validate_cutover_readiness(preflight_manifest) -> Result

The lane input bundle should contain only repository-relative paths in JSON. Internal Python code may use `Path` values to write files, but those absolute or workspace-local paths must not be serialized into artifacts, prompts, logs intended for retention, or final review output. If a diagnostic must describe a forbidden absolute path shape, use a placeholder such as `<absolute-local-path>`.

The external agent backend is configurable through environment variables and disabled by default. `SCHERZO_REVIEW_AGENT_COMMAND` contains the command template. `SCHERZO_REVIEW_AGENT_TIMEOUT_SECONDS` contains a positive integer timeout and defaults to 120 seconds when unset or invalid. The command template may use `{lane_id}`, `{prompt_path}`, `{bundle_dir}`, `{output_dir}`, and `{raw_output_path}` placeholders. The backend must write JSON to `{raw_output_path}`. The harness retains stdout and stderr regardless of success. Missing configuration, timeout, nonzero exit, malformed JSON, schema-invalid normalized output, dirty post-run working tree, or path containment violation produces a failed lane result.

The prompt contract for all lanes must include these common rules: inspect the actual diff; use the review brief only as orientation; inspect relevant repository files when available; cite repository-relative locations; keep findings and review notes distinct; do not mutate remote state; do not edit the working tree; emit JSON only; and include evidence references for every concrete finding. The harness enforces the parts of this contract that can be checked mechanically, including path containment, backend metadata, evidence validation, schema validation, and working-tree cleanliness.

The lane-agent response shape for external and fixture output should be smaller than `ReviewLaneResult` and should include:

    lane_id: correctness
    analysis_summary: short prose summary
    findings: list of proposed finding objects
    review_notes: list of proposed note objects
    evidence_requests: list of requested or scenario-provided evidence commands
    evidence_references: list of evidence keys or ids used by proposed findings

For correctness, find behavior bugs, broken invariants, inverted conditions, state-machine errors, parser/control mistakes, and regressions. Blocking findings require harness-issued executable evidence from a test, runtime observation, or reproduction. Static-only concerns are review notes.

For test quality, decide whether tests meaningfully exercise the changed behavior, not merely whether test files changed. Concrete weak or missing coverage can be a testing finding; uncertainty is a coverage note with a proposed test.

For idioms and maintainability, inspect production safety, error handling, module boundaries, public API shape, reviewability, and repository style. Concrete violations of production lint policy can be blockers; broad reviewability observations are notes.

For security and performance, inspect auth boundaries, secrets, command execution, filesystem and network parsing, process behavior, sleeps, polling, data growth, and hot paths. Concrete vulnerabilities or performance bugs are findings; broad boundary sensitivity is a risk note.

## Open Questions and Clarifications Needed

- [CLARIFY] The Linear issue description names `docs/plans/LIV-151-agent-backed-staged-review-lanes.md`, but the active workflow contract requires `docs/plans/LIV-152-<short-kebab-title>.md`. This draft follows the workflow contract and uses `docs/plans/LIV-152-agent-backed-staged-review-lanes.md`. Confirm whether a later rename or cross-reference is desired.
- [CLARIFY] Confirm the production external-agent command, model/provider selection, and credential handoff before enabling `--agent-backend external` by default. The shared harness contract, timeout handling, raw-output semantics, credential stripping, mutation detection, and failure artifacts are specified in this plan and can be implemented before that choice.
