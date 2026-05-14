# Configure Linear Completion States from Workflow Outcomes

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo runs workflows for Linear issues. A workflow can finish successfully from Scherzo's point of view because the agent produced the requested artifact, but that does not mean the human-facing Linear issue is accepted and complete. After this change, successful artifact-producing workflows move the Linear issue to a review state by default, while workflows that truly require no human review may move to a done state only when their policy explicitly allows that.

The observable operator outcome is that a workflow such as `workflow:execplan` can complete, publish its Markdown plan artifact or summary, and move the Linear issue to the configured review state, normally `In Review`, rather than `Done`. A low-risk no-artifact workflow can still move to `Done` when configured to say that no review is required. Failures, partial successes, and missing expected artifacts move to the configured attention state, normally `Needs Attention`; cancellations leave the issue unchanged unless a cancellation state is configured. The review and attention states are required Linear deployment config for the new policy. If a deployment lacks those states or uses different names, operators must configure the team-specific names or state ids and `doctor` must fail early until the Linear board and Scherzo config agree.

## Problem Framing and Constraints

Today the workflow lifecycle and Linear tracking lifecycle are too tightly coupled. In Scherzo terms, "workflow complete" means the worker reached a terminal runtime result. In Linear terms, "issue done" means the project team accepts the work. Those are not the same event for workflows that create code changes, documents, execution plans, retained run artifacts, or implementation summaries. Moving an issue straight to `Done` hides review work and can make operators believe an artifact has already been accepted.

This plan must keep the implementation proportionate. The goal is not to redesign all workflow execution. The goal is to add a small explicit outcome and policy layer between worker completion and Linear state updates. The layer must be configurable per workflow, must understand whether reviewable artifacts were produced, and must preserve legacy behavior when the new completion-state policy is absent. Once a deployment opts into the new policy, Scherzo should not provide a compatibility path for missing review or attention states. The config parser, Linear contract doctor check, and operator documentation must make those states required, fail before daemon operation when they are missing or ambiguous, and point operators to the doctor command and runbook remediation.

Repository constraints verified before writing this plan are that Scherzo is a Gleam project, the current handoff path has a `HandoffConfig` with `claim_state_id`, `success_state_id`, and `failure_state_id`, and `src/scherzo/handoff.gleam` currently calls `run_state_update` after success or failure comments. Production code must pass the repo's formatting and lint gates, including `glinter` and `scherzo_lint`; do not add production `let assert`, `panic`, or `todo`.

## Strategy Overview

Introduce a structured workflow completion outcome and a small policy module that maps that outcome to a Linear state decision. The outcome says whether the workflow succeeded, partially succeeded, failed, or was cancelled; whether it produced reviewable artifacts; whether review is required; whether expected artifacts were missing; and whether the workflow explicitly requested a target Linear state. The policy combines that outcome with global and per-workflow configuration and returns either "move to this Linear state" or "leave the Linear state unchanged," with a reason string suitable for logs and comments.

The Linear state transition should remain in the handoff/publish pipeline, not inside the workflow worker. Current code in `src/scherzo/handoff.gleam` already publishes comments or attachments and then calls `run_state_update` with `handoff_config.success_state_id` or `handoff_config.failure_state_id`. The new implementation should keep that shape: create the comment and any result attachment first, compute or carry the completion-state decision, then update the Linear state last. This preserves the separation between Scherzo lifecycle completion and external project tracking. If the Linear state update fails, Scherzo has still recorded that the worker finished; the publish side effect can report or retry the tracking update without reclassifying the workflow run as unfinished.

The configured policy is explicit, and a policy-enabled config must include the Linear state references used for review and attention outcomes. In the repository's expected deployment shape these references name `In Review` for reviewable success and `Needs Attention` for failure or missing-artifact attention. A team that uses different Linear state names must configure those names or state ids. Scherzo should reject the policy-enabled config or fail the Linear contract doctor check rather than pretending those states are optional.

For a successful workflow, move to the configured `default_completion_state` when the outcome requires review, when the workflow produced reviewable artifacts, or when workflow configuration marks the workflow as artifact-producing or review-required. In the standard config this is `In Review`, but the implementation must treat it as a required state reference from config, not as an unvalidated hard-coded assumption.

For a successful workflow, move to `Done` only when the workflow or policy explicitly says no review is required and `no_review_completion_state` is configured. A workflow with no artifacts does not automatically get `Done`; it must be configured as no-review or produce an outcome with `requires_review = false`.

For a successful workflow that was expected to produce reviewable artifacts but did not, treat the result as partial success and move to the configured `partial_success_state`. In the standard config this is `Needs Attention`. The success comment should explain that Scherzo completed but did not find the expected artifact.

For failure, move to the configured `failure_state`. In the standard config this is `Needs Attention`. If the policy block is present but the failure state is missing from config or absent from the Linear board, Scherzo should fail early through config validation or `doctor --check linear-contract`; it should not silently leave the issue unchanged as a supported deployment mode.

For cancellation, leave the Linear state unchanged by default, unless a `cancellation_state` is configured. Operator-initiated cancellation should not be reported as accepted work. If the existing result types cannot distinguish cancellation from failure, the first implementation milestone adds or derives a distinct cancellation classification before policy mapping.

For any explicit workflow target state, honor that target only if policy allows it and the state resolves unambiguously. Configured per-workflow target states must be covered by the same config and doctor validation as global completion states. If a dynamic target name or id cannot be resolved at publish time despite earlier checks, the runtime guard must not mutate Linear and must emit an operator-visible diagnostic that says to run `scherzo doctor --check linear-contract` and read the completion-state runbook. A bad state name must not silently move an issue to `Done`.

## Alternatives Considered

The simplest alternative is to change the existing global `handoff.success_state_id` in deployment config from a done state to an in-review state. That is too blunt. It would still treat all successful workflows the same, would not help no-review workflows, and would not explain why missing artifacts or partial success should go somewhere else.

Another option is to make each workflow directly update Linear when it finishes. That is more dangerous because every workflow would need to know Linear details and because state changes could happen before comments or artifacts are published. It also makes it harder to keep retries, logging, and operator diagnostics consistent.

A larger option is to redesign workflow results and artifact storage end to end. That may eventually be useful, but this ticket only needs configurable completion-state behavior. A small outcome type plus policy layer is enough to solve the immediate operator risk while keeping the existing handoff code path recognizable.

## Risks and Countermeasures

A workflow could be moved to the wrong Linear state because different teams use different state names. The implementation must represent configured states as a state reference, not just a string blindly sent to Linear. A state reference may be a legacy state id or a state display name. Name resolution must be explicit and fail early: policy-enabled config must require review and attention state references, `doctor --check linear-contract` must validate configured names or ids against the Linear board, and unresolved or ambiguous runtime lookups must refuse the mutation and produce an operator-visible diagnostic that points to doctor and the completion-state runbook.

Existing installations may rely on `handoff.success_state_id` pointing at their current done state. Preserve legacy behavior when the new completion-state policy is absent. The migration path is to add the new policy config with validated review and attention states, run doctor, and then remove or stop using legacy success-state id fields for workflows that should use artifact-aware defaults. Tests must prove that old configs still call the same Linear mutation as before, while policy-enabled configs fail validation when required completion states are missing.

The artifact-aware fallback could misclassify workflows if the runner cannot detect artifacts reliably. Counter this with two inputs: actual outcome artifacts when available, and per-workflow config such as `produces_reviewable_artifacts = true` or `requires_review = true`. For artifact-producing workflows, absence of artifacts is an attention condition, not an excuse to mark the issue done.

State updates could fail after comments or attachments are published. This is already possible in the current handoff shape because state update happens after comment work. The new policy should keep the same order and should report the failed state update as a publish/tracker problem, not as a worker lifecycle failure. Recovery remains to retry or manually update Linear.

The change could add too many config options. Keep the public shape small: required global review and attention targets, optional no-review and cancellation targets, optional per-workflow overrides, and legacy id compatibility. Avoid adding state machines or workflow-specific Linear code.

Failing early on missing Linear states can block rollout if the board is not prepared. That is intentional for policy-enabled deployments, but it must be easy to remediate. The countermeasure is explicit doctor output and a runbook that tells operators to either create the `In Review` and `Needs Attention` states or configure the team-specific state names or ids before enabling handoff completion policy.

## Progress

- [x] (2026-05-13 00:00Z) Drafted this ExecPlan from Linear issue LIV-281 and a focused inspection of the current repository.
- [x] (2026-05-13 00:30Z) Hardened the plan after review incorporation by closing config syntax ambiguity, state-reference parsing ambiguity, and artifact-detection ambiguity.
- [x] (2026-05-14 03:23Z) Incorporated the LIV-281 clarification that review and attention states are required Linear config for policy-enabled deployments, validated by doctor, and not an optional runtime compatibility case.
- [ ] Add policy and outcome types without changing existing behavior.
- [ ] Add config parsing for global defaults and per-workflow overrides while preserving legacy `handoff.*_state_id` behavior.
- [ ] Route worker success, partial success, failure, and cancellation through the policy layer.
- [ ] Update handoff publishing so comments and attachments are published before the selected Linear state transition.
- [ ] Add unit, config, handoff, and orchestration tests for policy mapping and no-regression behavior.
- [ ] Update operator-facing documentation and example config.
- [ ] Run the validation commands and record outcomes here.

## Surprises & Discoveries

- Observation: Current handoff config stores Linear target states as optional ids named `claim_state_id`, `success_state_id`, and `failure_state_id`, not as state names.
  Evidence: `src/scherzo/config/types.gleam` defines those fields on `HandoffConfig`, and `src/scherzo/config.gleam` parses them from the `handoff` config block.
- Observation: Current success and failure reporting already centralizes Linear comments, attachments, and state updates in the handoff client.
  Evidence: `src/scherzo/handoff.gleam` defines `report_success` and `report_failure`; both publish comments and then call `run_state_update` with the configured state id.

## Decision Log

- Decision: Put the target-state decision in a new policy layer and execute the state transition in the existing handoff/publish path.
  Rationale: This separates Scherzo worker lifecycle from Linear issue lifecycle while reusing the one place that already owns Linear comments, attachments, and state mutations.
  Date: 2026-05-13.
- Decision: Preserve legacy `handoff.success_state_id` and `handoff.failure_state_id` behavior when new completion-state policy config is absent.
  Rationale: The safer migration is additive. Existing deployments should not suddenly stop moving issues the way they are configured today.
  Date: 2026-05-13.
- Decision: Artifact-producing success defaults to `In Review`; `Done` is allowed only for explicitly no-review workflows.
  Rationale: This answers the operator safety problem directly. Reviewable artifacts need acceptance after generation, while no-review automation can still close issues when configured.
  Date: 2026-05-13.
- Decision: New completion-state config uses paired name and id keys rather than an overloaded string prefix. Keys ending in `_state` or `_completion_state` parse non-empty display names into `StateByName`, and sibling keys ending in `_state_id` or `_completion_state_id` parse non-empty Linear ids into `StateById`. Supplying both forms for the same target is invalid.
  Rationale: Existing config already uses explicit `*_state_id` fields, so paired keys preserve that style and avoid making operators memorize a new `state_id:` string convention.
  Date: 2026-05-13.
- Decision: The first implementation treats a non-empty `ResultArtifact` final or structured response as the minimal reviewable artifact signal and uses `StepArtifact` structured-output metadata paths when those paths are already available.
  Rationale: This avoids a broad artifact-store redesign while still detecting the operator safety case: a workflow configured to produce reviewable output must not be marked accepted when it returns no human-inspectable result.
  Date: 2026-05-13.
- Decision: Treat review and attention Linear states as required deployment config for the new completion-state policy instead of supporting missing-state deployments at runtime.
  Rationale: Scherzo's compatibility posture is to keep legacy configs working when the new policy is absent, but fail early once an operator opts into new behavior. Doctor and runbook remediation are safer than silently leaving policy-enabled issues in surprising states.
  Date: 2026-05-14.

## Outcomes & Retrospective

This section is intentionally empty at plan creation. During implementation, update it after each milestone with what changed, which validation commands passed, and any behavior that still needs follow-up.

## Context and Orientation

Scherzo is a Gleam application that polls Linear, chooses workflows for issues, starts workers, records runtime state, and publishes handoff information back to Linear. In this plan, "workflow lifecycle" means the internal Scherzo worker result: the worker succeeded, failed, was cancelled, or reached a partial success state. "Linear lifecycle" means the state shown on the Linear issue, such as `Todo`, `In Progress`, `In Review`, `Done`, or `Needs Attention`.

The files most relevant to this change are:

`src/scherzo/config/types.gleam` defines runtime config records. It currently includes `HandoffConfig` with optional `claim_state_id`, `success_state_id`, and `failure_state_id` fields.

`src/scherzo/config.gleam` constructs default config and parses the config file. It currently defaults handoff state ids to `None` and parses `handoff.claim_state_id`, `handoff.success_state_id`, and `handoff.failure_state_id` from the repository's existing YAML-like `yay` config syntax.

`src/scherzo/handoff.gleam` defines the handoff `Client`. When handoff is enabled, `report_success` publishes the success comment and optional result attachment, then calls `run_state_update` using `handoff_config.success_state_id`. `report_failure` publishes the failure comment and then calls `run_state_update` using `handoff_config.failure_state_id`. `claim_issue` similarly uses `claim_state_id`.

`src/scherzo/handoff_format.gleam` formats the human-readable Linear comments. It should be updated so the success or warning comment can say why Scherzo chose `In Review`, `Done`, `Needs Attention`, or no state change.

`src/scherzo/orchestrator/daemon.gleam` receives worker completions. The `Message` type includes `WorkerFinished`, and the message handler calls `worker_finished_to_transition`. This is the bridge from actual worker termination to orchestration transitions.

`src/scherzo/orchestrator/transition_types.gleam` defines transition messages, including `WorkerFinished`. The structured completion outcome can be introduced here or in a new module and then carried by the existing transition.

`src/scherzo/agent/types.gleam` defines worker success and failure data used by handoff; `src/scherzo/handoff.gleam` currently imports it as `agent_types`. The new outcome type should be derived from or embedded in these worker result types.

`src/scherzo/result_artifact.gleam` and `src/scherzo/step_artifact.gleam` are the artifact-related modules visible in the source tree. `src/scherzo/result_artifact.gleam` defines `ResultArtifact` with display and structured response text, a `source` string, truncation flags, and tool-call summaries. `src/scherzo/step_artifact.gleam` defines structured output metadata with a repository-relative `path` field and has code that returns a `result_artifact.ResultArtifact` from step artifacts. Use these modules to decide whether a workflow produced reviewable output rather than inventing an unrelated artifact model.

`src/scherzo/linear.gleam` owns Linear transport request construction and response parsing. State-name resolution, if it needs a new Linear query, belongs near existing Linear request helpers rather than inside the policy module.

`src/scherzo/linear_contract.gleam` validates Linear-related configuration. It already references handoff state bindings and must be extended so `doctor --check linear-contract` verifies every configured completion-state name or id for policy-enabled handoff. Missing `In Review`, missing `Needs Attention`, team-specific name mismatches, ambiguous names, or nonexistent ids should appear as contract diagnostics before daemon operation.

`docs/GETTING_STARTED.md` documents Linear contract checks and handoff policy. This plan should update that page and add a focused runbook at `docs/runbooks/linear-completion-states.md` so operator-facing errors can point to a concrete remediation document.

Existing tests that should guide implementation include `test/handoff_test.gleam`, `test/config_test.gleam`, `test/schema_guardrail_test.gleam`, `test/linear_contract_test.gleam`, `test/orchestrator_core_test.gleam`, `test/agent_runner_test.gleam`, and `test/agent_worker_control_test.gleam`.

## Preconditions and Verified Facts

The repository root contains `src/scherzo/config.gleam`, `src/scherzo/config/types.gleam`, `src/scherzo/handoff.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/transition_types.gleam`, `src/scherzo/result_artifact.gleam`, `src/scherzo/step_artifact.gleam`, `src/scherzo/linear.gleam`, and the test files named above. The repository also contains `docs/GETTING_STARTED.md` and a `docs/runbooks/` directory for operator documentation.

`src/scherzo/config/types.gleam` currently defines `HandoffConfig` with these fields relevant to this plan: `enabled`, `comment_on_claim`, `comment_on_success`, `comment_on_failure`, `comment_on_park`, `claim_state_id`, `success_state_id`, `failure_state_id`, `include_result_on_success`, `attach_result_on_success`, `attachment_fallback_to_markdown_link`, and `result_max_chars`.

`src/scherzo/config.gleam` defaults `success_state_id` and `failure_state_id` to `None` and parses non-empty string values for `handoff.success_state_id` and `handoff.failure_state_id`.

`docs/GETTING_STARTED.md` currently explains that `linear_contract` lets `doctor` compare config to the real Linear board before enforcing routing or handoff policy, and it shows `linear_contract.required_states` plus handoff bindings as the operator-facing contract shape.

`src/scherzo/handoff.gleam` updates Linear state after comments and attachments. That order should remain unless a test proves it is impossible to keep.

`src/scherzo/result_artifact.gleam` currently exposes `structured_final_response` and stores the result `source`. `src/scherzo/step_artifact.gleam` currently exposes `workflow_result_artifact` and structured-output metadata with `artifact_name`, `format`, `ref`, `path`, `sha256`, `bytes`, and validation status. A first implementation can detect a reviewable result without adding a new artifact store: non-empty final or structured response means Scherzo has human-inspectable output, and existing structured-output metadata can supply a path when the caller already has step artifacts.

The workspace started clean according to the source-control status command required by the workflow contract.

## Scope Boundaries

In scope: add a structured completion outcome, add an outcome-to-Linear-state policy, parse global and per-workflow completion-state configuration, require policy-enabled review and attention state references, preserve legacy handoff state-id behavior when the policy is absent, update handoff publishing to use the policy decision, validate configured completion states through doctor, and document operator-facing behavior.

In scope: add tests for policy mapping, state-name or state-id resolution, config failures for missing required completion states, doctor/Linear contract diagnostics for missing or ambiguous Linear states, handoff Linear mutations, missing-artifact behavior, failure and cancellation behavior, and legacy no-regression behavior.

Out of scope: changing how Scherzo creates workspaces, changing Linear polling or dispatch eligibility, changing how workflow labels are selected, changing the entire artifact store, or implementing a new Linear command system. This plan may ask the implementer to reuse artifact summaries that already exist or add a minimal summary if needed, but it must not turn into a broad artifact-store redesign.

Out of scope: removing `handoff.success_state_id` and `handoff.failure_state_id` immediately. Those legacy fields remain supported during migration.

Out of scope: supporting policy-enabled deployments whose configured review or attention states are absent from Linear. Those deployments must fail early and be fixed by config or board changes.

## Milestones

Milestone 1 adds pure data types and a pure policy function. At the end of this milestone, tests can call the policy with fake outcomes and fake config and observe the correct decision without hitting Linear. This reduces risk early because the default-policy question is answered before any GraphQL or orchestration code changes.

Milestone 2 adds config parsing and validation. At the end of this milestone, a config can specify required global targets such as `default_completion_state: In Review`, `failure_state: Needs Attention`, and `partial_success_state: Needs Attention`, plus optional targets such as `no_review_completion_state: Done` and per-workflow overrides. Legacy configs without the new block still parse and behave as before. Policy-enabled configs missing the required review or attention targets fail with clear config errors.

Milestone 3 wires worker results into structured outcomes. At the end of this milestone, success, failure, cancellation, partial success, and missing expected artifacts are represented before handoff publishing. The worker lifecycle remains separate from Linear state changes.

Milestone 4 updates handoff publishing. At the end of this milestone, success comments and attachments are still published first, then the policy-selected Linear state update is attempted. The comment or logs explain the selected state and reason.

Milestone 5 completes integration tests, doctor diagnostics, docs, migration notes, and validation. At the end of this milestone, an operator can read the docs, understand why an issue moved to `In Review` instead of `Done`, know how to configure no-review workflows, and know how to fix doctor failures when their Linear team uses different review or attention state names.

## Plan of Work

Create a new module `src/scherzo/workflow_completion_policy.gleam`. Keep it pure. Define the completion status, review requirement, state reference, workflow outcome, policy config, per-workflow override, and decision types. The module should not import Linear transport. It should accept plain values and return a plain decision.

Add config types to `src/scherzo/config/types.gleam`. Extend `HandoffConfig` with an optional completion policy field. The policy record itself must require `default_completion_state`, `failure_state`, and `partial_success_state`, because a policy-enabled deployment without those targets is invalid. Keep `no_review_completion_state` and `cancellation_state` optional. Each value should parse to a `LinearStateRef`, not a raw string. Preserve the existing `claim_state_id`, `success_state_id`, and `failure_state_id` fields.

Add parser support in `src/scherzo/config.gleam`. The parser must use the repository's existing YAML-like `yay` config syntax. State display-name keys parse to `StateByName`; state id keys parse to `StateById`. For each target, supplying both the display-name key and the id key is invalid. When `handoff.completion_states` is present, the parser must require one name-or-id value for each of `default_completion_state`, `failure_state`, and `partial_success_state`. It must also require `linear_contract.enabled = true` so doctor is available to validate the configured Linear states before daemon operation. The parser should accept this shape:

    linear_contract:
      enabled: true
    handoff:
      enabled: true
      completion_states:
        default_completion_state: In Review
        no_review_completion_state: Done
        failure_state: Needs Attention
        partial_success_state: Needs Attention
        workflows:
          execplan:
            produces_reviewable_artifacts: true
            requires_review: true
          no-review-maintenance:
            produces_reviewable_artifacts: false
            requires_review: false
            success_state: Done

The same target may be configured by id instead of display name with an `_id` sibling key, such as `default_completion_state_id`, `no_review_completion_state_id`, `failure_state_id`, `partial_success_state_id`, `cancellation_state_id`, and per-workflow `success_state_id`. Do not use an overloaded string prefix such as `state_id:<id>` for the first implementation. Empty names or ids are invalid, missing required review or attention keys are invalid, and parser errors must name the exact key path. Do not add an `unresolved_state_policy` config escape hatch; unresolved states are a doctor/config failure, not a supported policy choice.

In `src/scherzo/workflow_completion_policy.gleam`, implement the policy in this order:

First, if the outcome has an explicit `target_linear_state`, return that target unless the workflow policy forbids explicit targets. The first implementation may allow explicit targets by default because the workflow produced them deliberately, but configured explicit targets must still be validated by config and doctor, and dynamic targets must fail safe if resolution later fails.

Second, if status is cancelled, use `cancellation_state` only when configured; otherwise return `LeaveLinearState("workflow was cancelled")`.

Third, if status is failure, choose the required `failure_state` when the policy is present. When the policy is absent, keep the legacy `failure_state_id` fallback behavior.

Fourth, if status is partial success or `expected_artifacts_missing` is true, choose the required `partial_success_state`. If policy validation allowed this field to be absent, that is a bug in the implementation.

Fifth, if status is success and review is required by outcome, produced artifacts, or per-workflow config, choose the required `default_completion_state`. If policy validation allowed this field to be absent, that is a bug in the implementation.

Sixth, if status is success and review is explicitly not required, choose `no_review_completion_state` only when configured. If no no-review state is configured, leave unchanged. This prevents an accidental move to `Done` from a missing optional config value.

Seventh, if status is success and review requirement is unknown, treat it as review required when artifacts exist or the workflow is configured as artifact-producing. Otherwise leave unchanged unless the per-workflow override explicitly chooses a success state.

Add state-name resolution near the Linear layer. The policy returns a `LinearStateRef`, such as `StateById("...")` or `StateByName("In Review")`. `src/scherzo/handoff.gleam` or a helper in `src/scherzo/linear.gleam` resolves `StateByName` to the id needed by the Linear update mutation. If the issue's team state list is already available in the tracker issue data, use it. If it is not available, add a small Linear query for the issue's team workflow states and parse the response. Cache only if existing transport patterns already have safe caching; otherwise keep the first implementation simple. Resolution failure is not a supported compatibility mode: it should produce a typed publish/tracker diagnostic, skip the mutation, and point the operator to `scherzo doctor --check linear-contract` and `docs/runbooks/linear-completion-states.md`.

Update `src/scherzo/handoff.gleam`. Change the `Client.report_success` and `Client.report_failure` signatures only as much as necessary to receive the structured outcome or a computed decision. Do not make workflow workers call Linear directly. In `report_success`, compute the comment options, publish the success comment, attach the result if configured, and then call a new helper such as `run_completion_state_update` with the policy decision. In `report_failure`, publish the failure comment and then call the same helper. The helper should no-op for intentional `LeaveLinearState` decisions such as cancellation with no configured target, should call the existing state update path for resolved ids, and should treat unresolved required configured states as an operator-facing publish/tracker error rather than a normal no-op.

Update `src/scherzo/handoff_format.gleam` so comments can include a short line such as `Tracking state: moved to In Review because reviewable artifacts were produced.` or `Tracking state: left unchanged because no cancellation state is configured.` Keep the message concise; operators need to know why the state changed or did not change.

Update orchestration code in `src/scherzo/orchestrator/daemon.gleam` and `src/scherzo/orchestrator/transition_types.gleam` so `WorkerFinished` handling derives a `WorkflowCompletionOutcome` before handoff. Reuse existing worker success and failure records from `src/scherzo/agent/types.gleam`. If cancellation is not currently distinct, add the smallest representation needed to classify operator cancellation separately from ordinary failure. [CLARIFY] The implementer should confirm the current cancellation representation in `src/scherzo/agent/types.gleam` and related worker-control tests before choosing whether to add a new field or derive the status from an existing reason.

Update artifact derivation. Prefer existing result artifact data from `src/scherzo/result_artifact.gleam` and `src/scherzo/step_artifact.gleam`. A reviewable artifact is any produced item intended for a human to inspect, including source changes, documentation changes, an ExecPlan Markdown file, retained run artifacts, or implementation summaries. In the first implementation, build at least one `CompletionArtifactSummary` when `result_artifact.structured_final_response(success.result)` or `success.result.final_response` is present and not an empty string after trimming; set its `kind` to `success.result.source`, its `path` to `None`, and `reviewable` to `True`. When outcome derivation already has `StepArtifact` structured output metadata, create summaries with `path: Some(metadata.path)` for `StructuredOutputValid` values. If a workflow is configured as `produces_reviewable_artifacts = true` but neither result text nor structured-output metadata is present, mark `expected_artifacts_missing = true`.

Update `src/scherzo/linear_contract.gleam` to validate the new state names and ids. This is required, not optional. The operator check must say which configured completion states are required and whether Linear has matching states for the team/project. Missing names, ambiguous names, and nonexistent ids should produce diagnostics whose source names the config key, such as `handoff.completion_states.default_completion_state`, and whose message tells the operator to run `scherzo doctor --check linear-contract` after fixing the board or config.

Update documentation. Add a short operator-facing section to `docs/GETTING_STARTED.md` near the Linear contract and handoff policy sections, and add `docs/runbooks/linear-completion-states.md`. The docs must state that policy-enabled handoff requires configured review and attention states, that the standard names are `In Review` and `Needs Attention`, that teams with different names should use their names or state ids, and that operators must run `scherzo doctor --check linear-contract` before enabling daemon handoff. Do not bury the default-policy answer only in tests.

## Concrete Steps

From the repository root, start by approving direnv if needed. If `direnv exec . <command>` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry through direnv.

1. Run the focused baseline tests that already cover handoff and config:

       direnv exec . gleam test test/handoff_test.gleam test/config_test.gleam test/orchestrator_core_test.gleam

   Expect the command to compile and the named tests to pass before implementation. If this exact multi-file invocation is not accepted by the test runner, run `direnv exec . gleam test` and record the current pass count in this Progress section.

2. Add `src/scherzo/workflow_completion_policy.gleam` with pure types and a stubbed policy function. Do not call Linear from this module.

3. Add `test/workflow_completion_policy_test.gleam` with failing tests for these cases: artifact-producing success chooses configured `In Review`; explicit no-review success chooses configured `Done`; success with no no-review state leaves unchanged; missing expected artifacts chooses configured `Needs Attention`; failure chooses configured `Needs Attention`; cancellation leaves unchanged by default; and explicit target state wins. Do not add a policy-level unresolved-state fallback test; unresolved names and ids are handled by config, doctor, and the Linear-layer resolver.

4. Run the new policy test and expect failures for the unimplemented mapping:

       direnv exec . gleam test test/workflow_completion_policy_test.gleam

5. Implement the policy mapping until the new policy tests pass.

6. Extend `src/scherzo/config/types.gleam` with completion policy config records. Keep legacy fields intact.

7. Extend `src/scherzo/config.gleam` to parse `handoff.completion_states` and per-workflow overrides. Add a small helper that takes a display-name key path and an id key path, rejects empty values, rejects both keys being present for the same target, and returns `Option(LinearStateRef)`. For required global targets, wrap that helper with validation that turns missing `default_completion_state`, `failure_state`, or `partial_success_state` into an `InvalidConfig` error naming the missing key. Also reject policy-enabled config when `linear_contract.enabled` is false, with remediation text telling the operator to enable Linear contract doctor checks.

8. Add or update `test/config_test.gleam` to assert that the example config parses display-name fields to `StateByName`, id fields to `StateById`, duplicate name/id fields produce an `InvalidConfig` error naming the conflicting keys, missing required completion-state keys produce `InvalidConfig` errors naming the missing keys, policy-enabled config with `linear_contract.enabled = false` is invalid, and legacy config without `completion_states` still produces the old `success_state_id` and `failure_state_id` behavior.

9. Update `test/schema_guardrail_test.gleam` if this repository has schema guardrails for config shape. The test should prove the new config keys are included in the generated or checked schema.

10. Run config tests:

       direnv exec . gleam test test/config_test.gleam test/schema_guardrail_test.gleam

11. Add state reference resolution tests in `test/handoff_test.gleam`. Use the existing fake Linear transport style in that file. Cover `StateById`, resolvable `StateByName`, unresolved `StateByName`, and ambiguous or duplicate names if the resolver sees duplicate names. The unresolved and ambiguous cases must assert no Linear state mutation occurs and that the returned diagnostic points to `scherzo doctor --check linear-contract` and `docs/runbooks/linear-completion-states.md`.

12. Update `src/scherzo/linear.gleam` and `src/scherzo/handoff.gleam` with the minimal resolver and `run_completion_state_update` helper. Preserve existing `run_state_update` for id-based updates.

13. Update `src/scherzo/handoff_format.gleam` and its tests so comments can include the chosen tracking-state reason.

14. Run handoff tests:

       direnv exec . gleam test test/handoff_test.gleam

15. Add outcome derivation tests in `test/orchestrator_core_test.gleam` or a new targeted test file if the existing tests are too broad. Cover worker success with non-empty `ResultArtifact` text, worker success with `StepArtifact` structured-output metadata when that metadata is already available, success without expected artifact for an artifact-producing workflow, failure, and cancellation.

16. Update `src/scherzo/orchestrator/transition_types.gleam` and `src/scherzo/orchestrator/daemon.gleam` to carry or derive `WorkflowCompletionOutcome` and pass it to handoff.

17. Run orchestration and worker-control tests:

       direnv exec . gleam test test/orchestrator_core_test.gleam test/agent_runner_test.gleam test/agent_worker_control_test.gleam

18. Update `test/linear_contract_test.gleam` alongside `src/scherzo/linear_contract.gleam`. Verify configured completion-state names and ids are checked, verify missing `In Review` and `Needs Attention` states produce diagnostics with sources like `handoff.completion_states.default_completion_state` and `handoff.completion_states.failure_state`, and verify ambiguous names or nonexistent ids fail doctor instead of being tolerated.

19. Update `docs/GETTING_STARTED.md` and add `docs/runbooks/linear-completion-states.md` with the config example, the default policy answer, the doctor command, and remediation for teams whose review or attention states are named differently. Include migration guidance for `handoff.success_state_id` deployments.

20. Run full validation from the repository root:

       direnv exec . gleam format --check src test
       direnv exec . gleam test
       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

   Expect all commands to exit successfully. If any command fails, update this plan's Surprises & Discoveries with the exact failure and fix the implementation before continuing.

21. Commit after each green milestone. Suggested commit map: one commit for policy types and tests, one commit for config parsing, one commit for handoff state resolution, one commit for orchestration outcome wiring, and one commit for docs and final validation.

## Testing and Falsifiability

The core falsifiable claim is that successful artifact-producing workflows no longer go to `Done` by default. A policy test must construct a success outcome with one reviewable artifact and no explicit target state, pass a policy config whose required default completion state is `In Review`, and assert the decision is `MoveToState(StateByName("In Review"))` with a reason mentioning reviewable artifacts. The same test must fail if the decision is `Done` or unchanged.

A no-review test must construct a success outcome with `requires_review = false`, no reviewable artifacts, and `no_review_completion_state = "Done"`. It must assert that the decision is `Done`. A paired test with no `no_review_completion_state` must assert `LeaveLinearState`, proving that `Done` is not implicit.

A missing-artifact test must configure workflow `execplan` with `produces_reviewable_artifacts = true`, construct a success outcome with no artifacts, and assert that the decision is the required configured partial-success state, normally `Needs Attention`. This proves that Scherzo does not silently accept a workflow that finished without its expected output.

Failure tests must assert that ordinary worker failure maps to the required configured failure state, normally `Needs Attention`, when the new policy is enabled and that legacy `failure_state_id` is still used when the new policy is absent.

Cancellation tests must assert that cancellation leaves state unchanged by default. If a `cancellation_state` is configured, the test must assert that cancellation moves to that state instead.

Handoff tests must use fake Linear transport to prove ordering. For success with attachment enabled, the expected order is comment creation, attachment upload when configured, then state update. For an intentional `LeaveLinearState` decision, the fake transport must observe no state update mutation. For unresolved state names, the fake transport must observe the lookup attempt if a lookup is needed, no update mutation, and a diagnostic that references doctor and `docs/runbooks/linear-completion-states.md`.

No-regression tests must preserve existing behavior: with `handoff.enabled = true`, a legacy `handoff.success_state_id = "state-success"`, and no new completion-state policy block, successful handoff still sends a state update for `state-success` exactly as current tests expect.

Config tests must verify parsing of required global targets, per-workflow overrides, omitted optional states, display-name state references, id state references, and invalid values. Invalid values include empty state names, empty state ids, missing required `default_completion_state`, missing required `failure_state`, missing required `partial_success_state`, setting both name and id keys for the same target, policy-enabled config with `linear_contract.enabled = false`, and any unsupported `unresolved_state_policy` key. Expected parser errors should name the exact config key, such as `handoff.completion_states.default_completion_state`, or both conflicting keys when the name and id forms are both present.

Linear contract tests must verify doctor catches board/config mismatches for policy-enabled completion states. At minimum, one test should configure `default_completion_state: In Review` and `failure_state: Needs Attention` against a fake Linear board missing one of those states and assert a failing diagnostic with the corresponding `handoff.completion_states.*` source. A second test should use state ids and assert nonexistent ids fail before daemon operation.

Documentation is part of acceptance. A docs or config example test is not required, but the final review should verify the docs explain why `workflow:execplan` moves to `In Review` rather than `Done`, why `Needs Attention` is required for failure and missing-artifact outcomes, how to configure team-specific names or state ids, and how to run `scherzo doctor --check linear-contract` before enabling the policy.

## Validation and Acceptance

Run these commands from the repository root after implementation:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

Acceptance is behavioral, not just structural. With a policy-enabled config for `workflow:execplan`, a successful run that produces an ExecPlan Markdown artifact selects the configured review state, normally `In Review`, as the Linear target and explains that reviewable artifacts were produced. A workflow configured as no-review with no reviewable artifacts may select `Done`. A workflow that succeeds but does not produce an expected artifact selects the required configured partial-success state, normally `Needs Attention`. A failure selects the required configured failure state, normally `Needs Attention`. A cancellation leaves the issue unchanged unless a cancellation state is configured. A legacy config with only `handoff.success_state_id` behaves as it did before this change.

A policy-enabled config is accepted only when it supplies required review and attention state references and `doctor --check linear-contract` succeeds against the target Linear board. If a team lacks states named `In Review` or `Needs Attention`, acceptance requires either creating those states or configuring the team-specific names or state ids; leaving the deployment in a best-effort no-mutation mode is not accepted.

The implementation is not accepted if an artifact-producing success can move to `Done` solely because the workflow finished successfully. It is also not accepted if an unresolved configured state name falls back to `Done` or another terminal state, or if a policy-enabled config can run without required completion states and doctor validation.

## Rollout, Recovery, and Idempotence

Roll this out additively. First merge the code that supports the new policy while preserving legacy `handoff.*_state_id` fields. Then update repository or deployment config for artifact-producing workflows to opt into the new policy and set required completion states: `default_completion_state` to the team's review state, `failure_state` to the team's attention state, and `partial_success_state` to the team's attention state. In the standard deployment these are `In Review` and `Needs Attention`; other teams should configure their actual names or state ids. Run `scherzo doctor --check linear-contract` and do not enable daemon handoff until it passes. Finally, remove or stop relying on legacy success-state ids for workflows that should be artifact-aware.

Rollback is straightforward if the implementation keeps legacy fields intact. Disable or remove the `handoff.completion_states` block and Scherzo returns to the old `handoff.success_state_id` and `handoff.failure_state_id` behavior. If the code itself must be reverted, existing configs without the new block remain valid.

State updates should be idempotent. Re-running the same handoff publish should either set the issue to the same state again or intentionally leave it unchanged for cases such as cancellation with no configured target. Unresolved state names should not mutate Linear and should be treated as a misconfiguration that doctor should have caught. If a comment was published but state update failed, an operator can manually move the Linear issue, fix the config or board using the runbook, and retry the publish path if existing retry mechanisms support it.

## Artifacts and Notes

A concise operator-facing success comment should look like this in spirit:

    Scherzo workflow workflow:execplan completed successfully for run <run-id>.
    Result: created docs/plans/LIV-281-configurable-workflow-completion-states.md.
    Tracking state: moved to In Review because reviewable artifacts were produced.

A no-review workflow comment should make the explicit policy visible:

    Tracking state: moved to Done because this workflow is configured as no-review and produced no reviewable artifacts.

A missing-artifact case should not sound like accepted work:

    Tracking state: moved to Needs Attention because the workflow completed but expected reviewable artifacts were not found.

A cancellation with no configured state should be explicit:

    Tracking state: left unchanged because the workflow was cancelled and no cancellation state is configured.

A runtime state-resolution failure should point to remediation rather than pretending the missing state is supported:

    Tracking state: not updated because configured completion state `In Review` could not be resolved for this Linear team. Run `scherzo doctor --check linear-contract` and see `docs/runbooks/linear-completion-states.md`.

## Interfaces and Dependencies

In `src/scherzo/workflow_completion_policy.gleam`, define types equivalent to the following. Adjust names only if they conflict with existing style; keep the semantics stable.

    pub type CompletionStatus {
      CompletionSucceeded
      CompletionPartiallySucceeded
      CompletionFailed
      CompletionCancelled
    }

    pub type ReviewRequirement {
      ReviewRequired
      ReviewNotRequired
      ReviewUnknown
    }

    pub type LinearStateRef {
      StateById(String)
      StateByName(String)
    }

    pub type CompletionArtifactSummary {
      CompletionArtifactSummary(kind: String, path: Option(String), reviewable: Bool)
    }

    pub type WorkflowCompletionOutcome {
      WorkflowCompletionOutcome(
        status: CompletionStatus,
        artifacts: List(CompletionArtifactSummary),
        requires_review: ReviewRequirement,
        target_linear_state: Option(LinearStateRef),
        expected_artifacts_missing: Bool,
      )
    }

    pub type CompletionStateDecision {
      MoveToState(state: LinearStateRef, reason: String)
      LeaveLinearState(reason: String)
    }

    pub fn choose_linear_completion_state(
      policy: CompletionStatePolicy,
      workflow_id: String,
      outcome: WorkflowCompletionOutcome,
    ) -> CompletionStateDecision

In `src/scherzo/config/types.gleam`, add config records equivalent to:

    pub type CompletionStatePolicy {
      CompletionStatePolicy(
        default_completion_state: LinearStateRef,
        no_review_completion_state: Option(LinearStateRef),
        failure_state: LinearStateRef,
        partial_success_state: LinearStateRef,
        cancellation_state: Option(LinearStateRef),
        workflows: Dict(String, WorkflowCompletionOverride),
      )
    }

    pub type WorkflowCompletionOverride {
      WorkflowCompletionOverride(
        produces_reviewable_artifacts: Option(Bool),
        requires_review: Option(Bool),
        success_state: Option(LinearStateRef),
        no_review_completion_state: Option(LinearStateRef),
        failure_state: Option(LinearStateRef),
        partial_success_state: Option(LinearStateRef),
        cancellation_state: Option(LinearStateRef),
      )
    }

Do not add a policy that guesses a fallback state on unresolved names.

In `src/scherzo/config.gleam`, parse state references with paired keys. For example, `handoff.completion_states.default_completion_state` produces `StateByName("In Review")`, while `handoff.completion_states.default_completion_state_id` produces `StateById("<linear-state-id>")`. If both keys are present, return `error.InvalidConfig` and do not choose one silently. Apply the same helper to per-workflow `success_state` and `success_state_id` as well as the other per-workflow state override pairs. The global `default_completion_state`, `failure_state`, and `partial_success_state` pairs are required when `handoff.completion_states` is present.

The plan does not require new third-party dependencies. Use the existing Gleam standard library modules and Scherzo's existing Linear transport, config parser, result artifact, and test utilities.

## Open Questions and Clarifications Needed

- [CLARIFY] Confirm the exact current representation of operator cancellation in `src/scherzo/agent/types.gleam` and worker-control code. If cancellation is already distinguishable from failure, map it directly. If not, add the smallest field or constructor needed so policy can leave cancelled issues unchanged by default.
