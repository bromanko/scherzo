# Split domain.gleam into stabilized owner modules

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo runs automation from tracker issues. Operators need to reason about a tracker issue, a workflow definition, a workflow run, individual workflow steps, pi agent sessions, durable recovery checkpoints, and artifacts without all of those concepts being mixed together in one broad module. Today, `src/scherzo/domain.gleam` exports tracker records, configuration records, live session state, scheduler state, token counters, parked issue state, result artifacts, and runtime state from one place. That makes recovery and workflow terminology harder to stabilize because unrelated code can keep depending on the catch-all `domain` namespace.

After this plan is implemented, the same Scherzo daemon behavior, workflow behavior, tracker behavior, control protocol behavior, and external JSON shapes still work, but each type lives in the module that owns the concept. A developer can find tracker issue types under tracker modules, configuration types under config modules, workflow DAG types under workflow modules, live pi session state under session modules, agent run attempt state under agent modules, and orchestrator runtime state under orchestrator modules. The observable result is that `grep -R "import scherzo/domain" src test` and `grep -R "domain\." src test` find no remaining dependencies, while `direnv exec . gleam test` and `direnv exec . gleam format --check src test` pass.

## Problem Framing and Constraints

The Linear issue asks for a future replacement of a stale domain decomposition plan. The stale plan was removed because it predated the DAG-era workflow recovery model. The important operator problem is not merely that one file is large. The problem is that Scherzo now has more precise concepts: a workflow definition is a parsed directed acyclic graph, a workflow run is one execution of that graph for an issue, a workflow step is one agent or command node in the graph, a pi session is a live interaction with the pi coding agent, a checkpoint is durable state used to recover after restart, and an artifact is output captured from a step or run. If the code keeps routing these ideas through one broad `domain` module, future recovery work becomes easier to misunderstand and harder to audit.

This plan must not redesign workflow recovery. It must not change the DAG YAML grammar, tracker API payloads, control protocol payloads, pi protocol payloads, configuration file keys, hook behavior, workspace lifecycle behavior, orchestrator scheduling behavior, or handoff behavior. It is a type ownership refactor. Constructor names, field names, field order, JSON key names, and error codes must remain unchanged unless a later plan explicitly changes them.

The plan also has an executable timing gate. Implementation may begin only after the repository-local stabilization checks in `Preconditions and Verified Facts` pass. In plain terms, those checks prove that durable recovery records and recovery planning concepts live in `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, and `src/scherzo/state/recovery.gleam`, while workflow definitions, workflow runs, and step artifacts live in their existing owner modules rather than in `src/scherzo/domain.gleam`. If the checks find a new checkpoint, resumption, workflow-run, workflow-step, session, or artifact type in `src/scherzo/domain.gleam` or an unclear nearby owner, stop and revise this plan before moving any code. Do not treat the gate as passed by intuition alone.

## Strategy Overview

The chosen strategy is to move one coherent ownership group at a time, keep names and data shapes unchanged, update all imports and constructor qualifiers in the same slice, run structural greps, run focused tests, and commit only when the tree is green. This is proportionate because the desired outcome is navigability and terminology clarity, not a behavioral rewrite. Moving all types in one commit would hide mistakes. Renaming concepts at the same time would make it impossible to tell whether failures came from code motion or semantic changes.

The final ownership model is:

- `src/scherzo/tracker/issue.gleam` owns tracker issue records: `Issue` and `BlockerRef`.
- `src/scherzo/config/types.gleam` owns configuration records: `TrackerConfig`, `PollingConfig`, `WorkspaceConfig`, `HooksConfig`, `UiRequestPolicy`, `AgentConfig`, `PiConfig`, `HandoffConfig`, `LinearContractConfig`, `LinearCommandConfig`, `RoutingConfig`, `DagHooksConfig`, `ArtifactLimits`, `OrchestratorConfig`, and `EffectiveConfig`.
- `src/scherzo/result_artifact.gleam` owns `ResultArtifact`, next to the functions that create or consume result artifacts.
- `src/scherzo/session/tokens.gleam` owns `TokenTotals` and `zero_token_totals`, because token totals describe pi session and agent interaction accounting.
- `src/scherzo/session/live.gleam` owns `LiveSession`, the live pi process/session snapshot used by orchestration and session rendering.
- `src/scherzo/agent/run_attempt.gleam` owns `RunAttempt`, alongside the code that starts an agent attempt in a workspace.
- `src/scherzo/workspace/record.gleam` owns `WorkspaceRecord`, the durable workspace identity record.
- `src/scherzo/orchestrator/state.gleam` owns in-memory orchestrator state and scheduling records: `RetryEntry`, `RunningEntry`, `IssueCounter`, `new_issue_counter`, `ParkReleasePolicy`, `ParkedEntry`, `InvalidWorkflowReport`, and `RuntimeState`.

Existing owner modules that already hold stabilized concepts should stay in place. `src/scherzo/workflow_dag.gleam` already owns `WorkflowDag`, `WorkflowStep`, `StepKind`, `PromptRef`, `WorkspaceRef`, `FailurePolicy`, and `DagError`. `src/scherzo/workflow_run.gleam` already owns workflow-run execution result types. `src/scherzo/step_artifact.gleam` already owns step artifact status and records. `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, and `src/scherzo/state/recovery.gleam` already own durable record, projection, and recovery planning types.

## Alternatives Considered

The simplest alternative is to leave `src/scherzo/domain.gleam` alone and rely on comments or documentation to explain which type belongs where. That does not solve the navigation and audit problem: new code can still import `domain` and blur tracker, workflow, session, and orchestrator concepts.

Another option is a single large move that deletes `domain.gleam` in one commit. That is too risky because about forty source files currently import `scherzo/domain`, and compile errors alone would not distinguish accidental behavior changes from import churn.

A third option is to combine the move with terminology renames such as renaming fields or constructors to match the latest recovery language. That is intentionally rejected. The Linear issue says terminology must be stabilized before the split, but the accepted future plan should preserve behavior and external shapes unless it explicitly changes them. This plan closes the ownership decision but does not change semantics.

## Risks and Countermeasures

The main risk is a broad compile break caused by moving constructors. In Gleam, custom type constructors are qualified by the module that defines them, so code that constructs `domain.Issue(...)` must be updated to construct `tracker_issue.Issue(...)` when `Issue` moves. Each slice therefore updates both imports and constructor qualifiers before running tests.

A second risk is changing external protocol shapes accidentally. Encoders and decoders in files such as `src/scherzo/control/protocol.gleam`, `src/scherzo/pi/protocol.gleam`, `src/scherzo/session/json.gleam`, and tracker-related files must only change module qualifiers. Existing tests for control protocol, pi protocol, session events, Linear integration helpers, handoff, and configuration must keep passing.

A third risk is circular imports. The countermeasure is to keep data-only type modules thin. For example, `src/scherzo/orchestrator/state.gleam` may import tracker issue, session live, session tokens, and orchestrator reason modules, but it must not import `src/scherzo/orchestrator/core.gleam`, `src/scherzo/orchestrator/service.gleam`, or `src/scherzo/orchestrator/daemon.gleam`.

A fourth risk is stale terminology. The countermeasure is the first milestone: re-audit the current tree after recovery work lands and update this plan before any code motion if the type inventory or owner map has drifted.

A fifth risk is downstream code outside this repository importing `scherzo/domain`. This plan resolves that risk by treating `scherzo/domain` as an internal application module, not a supported public API. The repository evidence is that `gleam.toml` describes Scherzo as "A Gleam service that runs pi agents from Linear issues" and keeps the Hex publishing metadata commented out, and a documentation search for `scherzo/domain` finds internal plans rather than supported API documentation. If implementation uncovers a real external consumer, stop before deletion and write a separate compatibility plan instead of adding a compatibility shim here.

## Progress

- [x] (2026-05-03 00:00Z) Drafted this ExecPlan from the current tree without implementing the refactor.
- [x] (2026-05-03 00:30Z) Incorporated adversarial review findings: made the stabilization gate executable, resolved `scherzo/domain` deletion as internal-only, added residual-module rules, made the import grep a required checklist, added duplicate-definition checks, and scoped final greps.
- [x] (2026-05-04 01:59Z) Executed the stabilization gate before code motion. `jj status --color=never` reported no changes. The `src/scherzo/domain.gleam` public export grep matched only the planned type/helper inventory, and the owner-module grep showed workflow DAG, workflow run, step artifact, durable state, recovery, and agent attempt concepts still live outside `domain.gleam`. The generated migration checklist contains 83 `import scherzo/domain` matches in 83 files and 1,156 `domain.` qualifier matches in the same 83 files. Source checklist paths: `src/scherzo/command_step.gleam`, `src/scherzo/workspace_run.gleam`, `src/scherzo/linear_attachment.gleam`, `src/scherzo/tracker.gleam`, `src/scherzo/linear.gleam`, `src/scherzo/handoff.gleam`, `src/scherzo/terminal/render.gleam`, `src/scherzo/agent/types.gleam`, `src/scherzo/agent/turn_loop.gleam`, `src/scherzo/agent/run_attempt.gleam`, `src/scherzo/agent/pi_rpc.gleam`, `src/scherzo/agent/runner.gleam`, `src/scherzo/state/recovery.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/handoff_format.gleam`, `src/scherzo/result_artifact.gleam`, `src/scherzo/pi/client.gleam`, `src/scherzo/pi/protocol.gleam`, `src/scherzo/workflow_policy.gleam`, `src/scherzo/runtime_bundle.gleam`, `src/scherzo/linear_triage.gleam`, `src/scherzo/linear_contract.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/template.gleam`, `src/scherzo/workspace.gleam`, `src/scherzo/control/protocol.gleam`, `src/scherzo/control/linear_transport.gleam`, `src/scherzo/smoke.gleam`, `src/scherzo/orchestrator/effect_runner.gleam`, `src/scherzo/orchestrator/core.gleam`, `src/scherzo/orchestrator/worker_registry.gleam`, `src/scherzo/orchestrator/service.gleam`, `src/scherzo/orchestrator/event_publisher.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/workflow_reloader.gleam`, `src/scherzo/step_artifact.gleam`, `src/scherzo/config.gleam`, `src/scherzo/session/hub.gleam`, `src/scherzo/session/event.gleam`, and `src/scherzo/session/json.gleam`. Test checklist paths: `test/runtime_bundle_test.gleam`, `test/linear_http_test.gleam`, `test/ctl_attach_render_test.gleam`, `test/workspace_run_test.gleam`, `test/orchestrator_daemon_linear_command_test.gleam`, `test/orchestrator_daemon_session_event_test.gleam`, `test/template_test.gleam`, `test/command_step_test.gleam`, `test/session_event_test.gleam`, `test/workflow_policy_test.gleam`, `test/smoke_test.gleam`, `test/agent_runner_test.gleam`, `test/orchestrator_daemon_control_test.gleam`, `test/linear_triage_test.gleam`, `test/workflow_scheduler_test.gleam`, `test/linear_command_transport_test.gleam`, `test/orchestrator_effect_runner_test.gleam`, `test/linear_contract_test.gleam`, `test/orchestrator_service_doctor_test.gleam`, `test/linear_attachment_graphql_test.gleam`, `test/orchestrator_event_publisher_test.gleam`, `test/orchestrator_core_test.gleam`, `test/control_server_test.gleam`, `test/config_test.gleam`, `test/orchestrator_daemon_test.gleam`, `test/workspace_test.gleam`, `test/linear_attachment_test.gleam`, `test/handoff_test.gleam`, `test/session_hub_test.gleam`, `test/step_artifact_test.gleam`, `test/agent_worker_control_test.gleam`, `test/orchestrator_worker_registry_test.gleam`, `test/orchestrator_service_test.gleam`, `test/terminal_render_test.gleam`, `test/control_protocol_test.gleam`, `test/handoff_format_test.gleam`, `test/workflow_run_test.gleam`, `test/linear_comments_test.gleam`, `test/execplan_implementation_helper_test.gleam`, `test/state_recovery_test.gleam`, `test/ctl_test.gleam`, `test/linear_test.gleam`, and `test/domain_test.gleam`.
- [x] (2026-05-04 02:04Z) Moved tracker issue ownership to `src/scherzo/tracker/issue.gleam` and updated all `domain.Issue` and `domain.BlockerRef` references to `tracker_issue` qualifiers.
- [x] (2026-05-04 02:04Z) Moved configuration ownership to `src/scherzo/config/types.gleam` and updated config, workflow, agent, orchestrator, Linear, workspace, and test consumers to use `config_types` qualifiers.
- [x] (2026-05-04 02:04Z) Moved `ResultArtifact`, `TokenTotals`, `LiveSession`, `RunAttempt`, and `WorkspaceRecord` to `src/scherzo/result_artifact.gleam`, `src/scherzo/session/tokens.gleam`, `src/scherzo/session/live.gleam`, `src/scherzo/agent/run_attempt.gleam`, and `src/scherzo/workspace/record.gleam`; updated source and test consumers to use owner modules.
- [x] (2026-05-04 02:04Z) Moved orchestrator runtime state ownership to `src/scherzo/orchestrator/state.gleam` and updated retry, running, issue counter, parked issue, invalid workflow report, and runtime state references to `orchestrator_state` qualifiers.
- [x] (2026-05-04 02:06Z) Removed all remaining `scherzo/domain` imports and `domain.` qualifiers, deleted `src/scherzo/domain.gleam`, split `test/domain_test.gleam` into owner tests, and ran final structural and full validation. Final structural greps for `import scherzo/domain`, `domain.`, and `src/scherzo/domain.gleam` in `src test gleam.toml` printed no matches. `direnv exec . gleam format --check src test` exited 0, and `direnv exec . gleam test` exited 0 with 588 passed and no failures.

## Surprises & Discoveries

- Observation: The current tree already has several owner modules for the stabilized recovery model, including workflow DAG, workflow run, step artifact, session, agent, and state recovery modules.
  Evidence: `src/scherzo/workflow_dag.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/step_artifact.gleam`, `src/scherzo/session/event.gleam`, `src/scherzo/agent/types.gleam`, `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, and `src/scherzo/state/recovery.gleam` exist in the current tree.
- Observation: `src/scherzo/domain.gleam` remains a broad module rather than a single bounded domain.
  Evidence: It currently exports tracker issue records, configuration records, result artifact records, live session records, token totals, workspace records, run attempt records, retry records, running records, issue counters, parked issue records, invalid workflow reports, and `RuntimeState`.
- Observation: Many source files still import `scherzo/domain`.
  Evidence: `grep` over `src` found imports in files including `src/scherzo/config.gleam`, `src/scherzo/tracker.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/orchestrator/core.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/service.gleam`, `src/scherzo/pi/protocol.gleam`, `src/scherzo/session/json.gleam`, `src/scherzo/control/protocol.gleam`, `src/scherzo/agent/run_attempt.gleam`, and `src/scherzo/agent/turn_loop.gleam`.
- Observation: `test/domain_test.gleam` currently tests several unrelated concepts and should be split along the same ownership boundaries as the source types.
  Evidence: It contains tests for issue/blocker records, tracker kind parsing, issue state keys, token totals, issue counters, parked issues, and runtime state collections.
- Observation: A mechanical qualifier migration must account for overlapping names such as `Issue` and `IssueCounter`.
  Evidence: The first `direnv exec . gleam test` after the broad qualifier update failed to compile because several `domain.IssueCounter` references had become `tracker_issue.IssueCounter`. Replacing those with `orchestrator_state.IssueCounter` fixed the compile error, and the subsequent full test run passed.
- Observation: The final full test run prints several Erlang crash reports from tests that intentionally panic worker processes, but the suite still passes.
  Evidence: `direnv exec . gleam test` ended with `588 passed, no failures` after crash reports from orchestrator and workflow crash-handling tests.

## Decision Log

- Decision: This document is a proposal-only ExecPlan and does not implement the refactor.
  Rationale: The workflow contract for LIV-24 asks for a checked-in ExecPlan proposal, not source, test, or config changes.
  Date: 2026-05-03
- Decision: Move types without changing constructor names, field names, field order, JSON keys, config keys, protocol tags, or scheduler behavior.
  Rationale: The acceptance criteria require preserving config, workflow, tracker, orchestrator behavior, and external protocol shapes unless a new plan explicitly changes them.
  Date: 2026-05-03
- Decision: Begin implementation with a re-audit milestone.
  Rationale: The issue explicitly says the split should occur after workflow recovery terminology stabilizes, and the current tree may continue to evolve before implementation.
  Date: 2026-05-03
- Decision: Use narrow owner modules rather than another aggregate replacement for `domain.gleam`.
  Rationale: Replacing `domain` with a differently named catch-all module would not solve the conceptual ownership problem.
  Date: 2026-05-03
- Decision: Use a repository-local stabilization gate instead of an open-ended stakeholder clarification before code motion.
  Rationale: The implementer needs a pass/fail check. The gate proves that durable recovery, workflow run, workflow DAG, step artifact, session, and agent-attempt concepts still live in their owner modules, and it forces a plan revision if new domain-owned recovery terminology appears.
  Date: 2026-05-03
- Decision: Treat `scherzo/domain` as internal application code and delete it after internal imports are removed.
  Rationale: `gleam.toml` describes Scherzo as a service, Hex publishing metadata is commented out, and repository documentation references to `scherzo/domain` are internal plans rather than public API guidance. A discovered external consumer would require a separate compatibility plan.
  Date: 2026-05-03
- Decision: During staged moves, keep the residual `src/scherzo/domain.gleam` module compiling by importing new owner modules only for still-unmoved record field types.
  Rationale: Gleam custom types are nominal. Duplicating moved definitions or leaving old constructors in `domain.gleam` would either break type compatibility or preserve the ambiguous namespace this plan is meant to retire.
  Date: 2026-05-03
- Decision: Make the initial `src` and `test` import/qualifier grep output the migration checklist.
  Rationale: The refactor is broad enough that a generated checklist is safer than relying on a stale source-only inventory. Each match must be closed or explicitly deferred to a named later slice.
  Date: 2026-05-03
- Decision: Do not create the slice commits named in the checked-in ExecPlan while running the LIV-61 workflow.
  Rationale: The workflow contract for this implementation explicitly says not to create jj/git commits because the publish step creates the final logical jj commit after review and validation. This overrides the ExecPlan's generic commit-point guidance without changing the source refactor itself.
  Date: 2026-05-04
- Decision: Use owner-module aliases consistently across migrated consumers: `tracker_issue`, `config_types`, `result_artifact`, `session_tokens`, `session_live`, `run_attempt`, `workspace_record`, and `orchestrator_state`.
  Rationale: Stable aliases make the new ownership boundaries visible at call sites and avoid replacing `domain` with another ambiguous aggregate module.
  Date: 2026-05-04

## Outcomes & Retrospective

Implementation completed the planned ownership split without changing external protocol, config, workflow, tracker, scheduler, handoff, or JSON shapes. `src/scherzo/domain.gleam` and `test/domain_test.gleam` were removed after every `domain` consumer was migrated to owner modules. The split tests now live in `test/tracker_issue_test.gleam`, `test/tracker_types_test.gleam`, `test/session_tokens_test.gleam`, and `test/orchestrator_state_test.gleam`.

The final structural checks proved the old namespace is gone: `grep -R -n "import scherzo/domain" src test`, `grep -R -n "domain\." src test`, and `grep -R -n "src/scherzo/domain.gleam" src test gleam.toml` all printed no matches. Final validation passed with `direnv exec . gleam format --check src test` exit status 0 and `direnv exec . gleam test` exit status 0 with 588 passed and no failures. The only implementation surprise was the `Issue` versus `IssueCounter` overlapping-name migration bug, which was caught by compilation before tests ran and fixed by routing issue counters to `src/scherzo/orchestrator/state.gleam`.

## Context and Orientation

Scherzo is a Gleam application. The package target is Erlang, and `gleam.toml` declares dependencies such as `gleam_stdlib`, `gleam_erlang`, `gleam_otp`, `gleam_json`, `gleam_http`, `gleam_httpc`, `simplifile`, `yay`, and `birl`, with `gleeunit` as a dev dependency. Commands in this plan should be run from the repository root.

Before implementation, the broad type module was `src/scherzo/domain.gleam`; it mixed tracker issue records, config records, artifacts, sessions, workspace records, run attempts, and orchestrator runtime state. After implementation, `src/scherzo/domain.gleam` no longer exists. The owner modules named in the Strategy Overview are the current source of truth for those types.

The current workflow-definition module is `src/scherzo/workflow_dag.gleam`. A workflow DAG is the parsed YAML workflow graph. It defines `WorkflowDag`, `WorkflowStep`, `StepKind`, `PromptRef`, `WorkspaceRef`, `FailurePolicy`, and `DagError`. A step is either an agent step, which sends a prompt to pi, or a command step, which runs a command.

The current workflow-execution module is `src/scherzo/workflow_run.gleam`. A workflow run is one execution of a workflow DAG for one tracker issue. It now imports owner modules for issue, configuration, artifact limit, run attempt, result artifact, and token types, while continuing to use `src/scherzo/step_artifact.gleam`, `src/scherzo/workflow_scheduler.gleam`, `src/scherzo/workspace_run.gleam`, `src/scherzo/agent/run_attempt.gleam`, and `src/scherzo/agent/types.gleam` for workflow execution behavior.

The current durable-state modules are under `src/scherzo/state/`. `src/scherzo/state/record.gleam` defines ledger records such as run and recovery events. `src/scherzo/state/projection.gleam` projects ledger records into status maps. `src/scherzo/state/recovery.gleam` builds recovery plans from durable records. These files are the recovery checkpoint area; this plan should not move those durable record types unless the re-audit shows they still depend on types that are being moved out of `domain.gleam`.

The current session modules are under `src/scherzo/session/`. `src/scherzo/session/event.gleam` defines session event names and statuses. `src/scherzo/session/json.gleam` encodes or decodes session data. `src/scherzo/session/hub.gleam` manages session event subscribers. `src/scherzo/session/live.gleam` now owns `LiveSession`, and `src/scherzo/session/tokens.gleam` now owns `TokenTotals` and `zero_token_totals`.

The current agent modules are under `src/scherzo/agent/`. `src/scherzo/agent/types.gleam` defines worker success, worker failure, and pi update types. `src/scherzo/agent/run_attempt.gleam` contains the code that runs an agent prompt in a workspace and now owns the `RunAttempt` record.

The current orchestrator modules are under `src/scherzo/orchestrator/`. They coordinate polling, dispatch, worker registry, retry scheduling, effects, control commands, and daemon lifecycle. In-memory orchestrator state such as running issues, retry entries, issue counters, parked issues, invalid workflow reports, and token aggregation now lives in `src/scherzo/orchestrator/state.gleam`.

## Preconditions and Verified Facts

Before writing this plan, `jj status --color=never` reported a clean working copy with no changes. A filename-collision check for `docs/plans/LIV-24-*` found no existing plan file for this issue. At implementation start, the source tree contained `src/scherzo/domain.gleam`, `src/scherzo/workflow_dag.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/command_step.gleam`, `src/scherzo/step_artifact.gleam`, `src/scherzo/session/event.gleam`, `src/scherzo/agent/types.gleam`, `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, and `src/scherzo/state/recovery.gleam`. After implementation, `src/scherzo/domain.gleam` no longer exists.

The pre-implementation `src/scherzo/domain.gleam` exported these type groups:

- Tracker issue group: `BlockerRef`, `Issue`.
- Config group: `TrackerConfig`, `PollingConfig`, `WorkspaceConfig`, `HooksConfig`, `UiRequestPolicy`, `AgentConfig`, `PiConfig`, `HandoffConfig`, `LinearContractConfig`, `LinearCommandConfig`, `RoutingConfig`, `DagHooksConfig`, `ArtifactLimits`, `OrchestratorConfig`, `EffectiveConfig`.
- Artifact and metric group: `ResultArtifact`, `TokenTotals`, `zero_token_totals`.
- Live session and run group: `LiveSession`, `RunAttempt`.
- Workspace group: `WorkspaceRecord`.
- Orchestrator runtime group: `RetryEntry`, `RunningEntry`, `IssueCounter`, `new_issue_counter`, `ParkReleasePolicy`, `ParkedEntry`, `InvalidWorkflowReport`, `RuntimeState`.

The initial source import audit found `import scherzo/domain` in these source files: `src/scherzo/template.gleam`, `src/scherzo/session/json.gleam`, `src/scherzo/smoke.gleam`, `src/scherzo/config.gleam`, `src/scherzo/session/event.gleam`, `src/scherzo/step_artifact.gleam`, `src/scherzo/terminal/render.gleam`, `src/scherzo/orchestrator/workflow_reloader.gleam`, `src/scherzo/pi/protocol.gleam`, `src/scherzo/session/hub.gleam`, `src/scherzo/pi/client.gleam`, `src/scherzo/tracker.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/result_artifact.gleam`, `src/scherzo/linear_attachment.gleam`, `src/scherzo/control/linear_transport.gleam`, `src/scherzo/workspace_run.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/linear_contract.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/core.gleam`, `src/scherzo/linear_triage.gleam`, `src/scherzo/control/protocol.gleam`, `src/scherzo/orchestrator/event_publisher.gleam`, `src/scherzo/command_step.gleam`, `src/scherzo/handoff_format.gleam`, `src/scherzo/runtime_bundle.gleam`, `src/scherzo/orchestrator/effect_runner.gleam`, `src/scherzo/orchestrator/worker_registry.gleam`, `src/scherzo/workspace.gleam`, `src/scherzo/workflow_policy.gleam`, `src/scherzo/agent/runner.gleam`, `src/scherzo/orchestrator/service.gleam`, `src/scherzo/linear.gleam`, `src/scherzo/handoff.gleam`, `src/scherzo/state/recovery.gleam`, `src/scherzo/agent/types.gleam`, `src/scherzo/agent/pi_rpc.gleam`, `src/scherzo/agent/run_attempt.gleam`, and `src/scherzo/agent/turn_loop.gleam`.

The stabilization gate for this plan is repository-local and must run before any code motion. First, run `grep -n "^pub type\|^pub fn zero_token_totals\|^pub fn new_issue_counter" src/scherzo/domain.gleam`. The output must contain only the public types and helpers listed above for `src/scherzo/domain.gleam`; any additional public domain-owned type or helper means the gate fails. Second, run `grep -n -E "^pub type|^pub fn" src/scherzo/workflow_dag.gleam src/scherzo/workflow_run.gleam src/scherzo/step_artifact.gleam src/scherzo/state/record.gleam src/scherzo/state/projection.gleam src/scherzo/state/recovery.gleam src/scherzo/agent/types.gleam src/scherzo/agent/run_attempt.gleam` and confirm that workflow DAG, workflow run, step artifact, durable state record, state projection, state recovery, and agent-attempt concepts still live in those owner modules. The gate passes only if `src/scherzo/domain.gleam` has no new checkpoint, resumption, workflow-run, workflow-step, session, artifact, or unclear owner concept beyond the owner map in this plan. If the gate fails, update this ExecPlan first and do not move types until every new concept has a named owner and validation path.

The deletion of `src/scherzo/domain.gleam` is an accepted part of this plan, not an unresolved public API question. The evidence is that `gleam.toml` describes this package as a service, its publishing metadata is commented out, and a repository documentation search for `scherzo/domain` finds internal planning references rather than public API documentation. If implementation discovers an actual downstream consumer outside this repository, stop before deleting `src/scherzo/domain.gleam` and write a separate compatibility plan.

If any verified facts differ when implementation begins, update this ExecPlan first. The implementation must follow the current tree, not this draft's stale assumptions.

## Scope Boundaries

In scope: moving exported types and helper functions out of `src/scherzo/domain.gleam`; updating imports, type annotations, constructors, and field-access qualifiers in `src` and `test`; splitting `test/domain_test.gleam` into owner-specific tests; deleting `src/scherzo/domain.gleam` and `test/domain_test.gleam` only after structural greps prove they are unused.

Out of scope: changing workflow DAG parsing, scheduler ordering, retry policy behavior, workspace creation/removal behavior, hook command behavior, Linear API behavior, control protocol behavior, pi protocol behavior, JSON field names, config keys, state ledger record formats, recovery semantics, attachment behavior, handoff behavior, or terminal rendering output. If implementation uncovers a necessary behavior change, stop and revise this plan before making the behavior change.

The current owner modules for workflow definitions and durable recovery records stay where they are. Do not move `WorkflowDag`, `WorkflowStep`, `StepKind`, `PromptRef`, `WorkspaceRef`, `FailurePolicy`, `DagError`, `LedgerRecord`, `RecordBody`, `Projection`, `RecoveryPlan`, `StepStatus`, or `StepArtifact` unless the re-audit shows they were newly moved into `domain.gleam` after this plan was written.

## Milestones

Milestone 1 executes the stabilization gate and builds the migration checklist. At the end of this milestone, the plan's verified facts and owner map match the implementation tree, `src/scherzo/domain.gleam` exports only the type groups named in this plan, recovery and workflow concepts still live in their owner modules, and the current `src` and `test` import/qualifier grep output has been recorded in `Progress` as the working checklist. This milestone comes first because the Linear issue explicitly says the split should happen only after workflow recovery terminology stabilizes. The milestone is accepted only when the gate passes; if the gate fails, the accepted outcome is a revised plan and no code motion.

Milestone 2 moves tracker issue ownership. At the end of this milestone, `Issue` and `BlockerRef` live in `src/scherzo/tracker/issue.gleam`, tracker-facing and Linear-facing modules import that owner module, and tests that construct issues still pass. This comes early because tracker issue records are widely used and are a clear bounded concept.

Milestone 3 moves configuration ownership. At the end of this milestone, all configuration records live in `src/scherzo/config/types.gleam`, `src/scherzo/config.gleam` remains responsible for loading and parsing configuration, and configuration tests prove keys and defaults did not change. This is separate because config types are widely referenced by orchestrator, workflow, and agent code.

Milestone 4 moves artifact, metric, session, run attempt, and workspace record ownership. At the end of this milestone, `ResultArtifact`, `TokenTotals`, `LiveSession`, `RunAttempt`, and `WorkspaceRecord` each live beside the code that owns their lifecycle. This keeps workflow run and pi session terminology explicit before moving the larger orchestrator state group.

Milestone 5 moves orchestrator runtime state ownership. At the end of this milestone, in-memory runtime collections and scheduling records live in `src/scherzo/orchestrator/state.gleam`, orchestrator modules depend on that module rather than `domain`, and retry, park, daemon, control, and recovery tests still pass.

Milestone 6 removes the old aggregate module. At the end of this milestone, no source or test file imports `scherzo/domain`, `src/scherzo/domain.gleam` has been deleted, split tests have replaced `test/domain_test.gleam`, full validation passes, and the retrospective records the final ownership map.

## Plan of Work

Start by running the stabilization gate and import inventory commands from the repository root. Update the `Preconditions and Verified Facts` section if `src/scherzo/domain.gleam` has gained, lost, or renamed types. Search recovery-related modules before moving anything. The minimum files to re-check are `src/scherzo/domain.gleam`, `src/scherzo/workflow_dag.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/step_artifact.gleam`, `src/scherzo/session/event.gleam`, `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, `src/scherzo/state/recovery.gleam`, `src/scherzo/agent/types.gleam`, and `src/scherzo/agent/run_attempt.gleam`. Then run the `src` and `test` greps for `import scherzo/domain` and `domain.`. Record the path list and match counts in `Progress`; this generated output is the migration checklist, and every match must be closed or explicitly deferred to a named later slice.

During staged moves, `src/scherzo/domain.gleam` is a temporary residual module for only the types not yet moved. After moving a type, delete its `pub type` or helper function from `domain.gleam`. If remaining records in `domain.gleam` still need that concept, import the new owner module into `domain.gleam` and change residual field annotations to owner-qualified types such as `tracker_issue.Issue`, `result_artifact.ResultArtifact`, `session_tokens.TokenTotals`, or `session_live.LiveSession`. Do not copy moved nominal types back into `domain.gleam`, do not create a re-export or compatibility namespace, and do not finish any slice with duplicate public constructors in both `domain.gleam` and the owner module. This rule is why each slice includes duplicate-definition greps.

For tracker issues, create `src/scherzo/tracker/issue.gleam`. Move the exact `BlockerRef` and `Issue` definitions from `src/scherzo/domain.gleam` into that file. Keep constructor names, field names, field order, and imports unchanged except that the new module should import `birl.{type Time}`, `gleam/option.{type Option}`, and `scherzo/tracker/state as issue_state` directly. Update source and tests so type annotations use `tracker_issue.Issue` or `tracker_issue.BlockerRef`, and constructor calls use `tracker_issue.Issue(...)` and `tracker_issue.BlockerRef(...)`.

For configuration, create `src/scherzo/config/types.gleam`. Move the exact configuration type definitions from `src/scherzo/domain.gleam` into that file. This module should import `gleam/dict.{type Dict}`, `gleam/option.{type Option}`, `scherzo/model_config`, `scherzo/tracker/kind as tracker_kind`, and `scherzo/tracker/state as issue_state`. Update `src/scherzo/config.gleam` to parse into `config_types.EffectiveConfig` and related records. Update orchestrator, workflow, agent, runtime bundle, and policy modules to import `scherzo/config/types as config_types` where they currently reference config records through `domain`.

For result artifacts, move `ResultArtifact` into `src/scherzo/result_artifact.gleam`. That file already exists and currently imports `scherzo/domain`; after the move, functions in the file should construct the local `ResultArtifact` constructor. Update consumers to import `scherzo/result_artifact` for both functions and the type.

For token totals, create `src/scherzo/session/tokens.gleam`. Move `TokenTotals` and `zero_token_totals` into that file. Update `src/scherzo/agent/types.gleam`, `src/scherzo/workflow_run.gleam`, orchestrator modules, and tests to use `session_tokens.TokenTotals` and `session_tokens.zero_token_totals()`.

For live sessions, create `src/scherzo/session/live.gleam`. Move `LiveSession` into that file. Update session JSON, terminal rendering, pi client/protocol code, orchestrator daemon/service/core code, and any control protocol code that displays or transports live session snapshots to import `scherzo/session/live as session_live`. Do not change any JSON keys or rendered text during this move.

For agent run attempts, move `RunAttempt` into `src/scherzo/agent/run_attempt.gleam`. That module already exists; add the public type near the top of the file and change its issue field to use the moved tracker issue type. Update code that constructs or annotates a run attempt to use `run_attempt.RunAttempt(...)`.

For workspace records, create `src/scherzo/workspace/record.gleam`. Move `WorkspaceRecord` there. Update workspace and state modules that persist or read workspace records to import `scherzo/workspace/record as workspace_record`.

For orchestrator runtime state, create `src/scherzo/orchestrator/state.gleam`. Move `RetryEntry`, `RunningEntry`, `IssueCounter`, `new_issue_counter`, `ParkReleasePolicy`, `ParkedEntry`, `InvalidWorkflowReport`, and `RuntimeState` into that file. The new module should import `gleam/dict.{type Dict}`, `gleam/option.{type Option}`, `birl.{type Time}`, `scherzo/orchestrator/reason`, `scherzo/tracker/issue as tracker_issue`, `scherzo/session/live as session_live`, and `scherzo/session/tokens as session_tokens`. Update orchestrator modules and recovery projection modules to use `orchestrator_state.RuntimeState`, `orchestrator_state.RunningEntry`, and the other moved constructors.

When `src/scherzo/domain.gleam` has no remaining types, delete it. A compatibility module or re-export is out of scope for this plan, because keeping `scherzo/domain` alive would preserve the ambiguous namespace this plan is meant to retire. If a real external consumer is discovered before deletion, stop and write a separate compatibility plan rather than changing this refactor into a public API migration.

## Concrete Steps

1. From the repository root, inspect source control state:

       jj status --color=never

   Expect a clean working copy or only this plan already modified. If unrelated files are modified, stop and ask the owner before implementing.

2. From the repository root, execute the stabilization gate:

       grep -n "^pub type\|^pub fn zero_token_totals\|^pub fn new_issue_counter" src/scherzo/domain.gleam
       grep -n -E "^pub type|^pub fn" src/scherzo/workflow_dag.gleam src/scherzo/workflow_run.gleam src/scherzo/step_artifact.gleam src/scherzo/state/record.gleam src/scherzo/state/projection.gleam src/scherzo/state/recovery.gleam src/scherzo/agent/types.gleam src/scherzo/agent/run_attempt.gleam

   The first command must show only the type and helper names listed in `Preconditions and Verified Facts` for `src/scherzo/domain.gleam`. The second command must show that workflow DAG, workflow run, step artifact, state record, state projection, state recovery, and agent-attempt concepts still live in their owner modules. If either output shows a new domain-owned checkpoint, resumption, workflow-run, workflow-step, session, artifact, or unclear owner concept, stop and update this ExecPlan before moving code.

3. From the repository root, generate the current consumer checklist:

       grep -R -n "import scherzo/domain" src test
       grep -R -n "domain\." src test

   Record the path list and match counts in `Progress` before editing source. Every `src` and `test` match is part of the migration checklist. Close each match in the slice that owns its referenced type, or explicitly mark it in `Progress` as deferred to a named later slice. For later greps described as "should print no matches," empty output is success even though `grep` may exit with status 1; do not record a slice as accepted if the grep prints any match.

4. Create `src/scherzo/tracker/issue.gleam` and move `BlockerRef` and `Issue` into it exactly as described in the Plan of Work.

5. In every file that constructs or annotates `domain.Issue` or `domain.BlockerRef`, add `import scherzo/tracker/issue as tracker_issue` and replace only those references with `tracker_issue.Issue` or `tracker_issue.BlockerRef`. If still-unmoved records in `src/scherzo/domain.gleam` need issue fields, import `scherzo/tracker/issue as tracker_issue` there and update those field annotations to owner-qualified types. Run:

       grep -n -E "^pub type (Issue|BlockerRef)" src/scherzo/domain.gleam
       grep -R -n -E "domain\.(Issue|BlockerRef)" src test
       direnv exec . gleam test

   The first grep proves the moved definitions are no longer duplicated in `src/scherzo/domain.gleam`; it should print no matches. The second grep should print no matches. The tests should exit successfully. If direnv reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the command.

6. Move the issue and blocker assertions from `test/domain_test.gleam` into a new `test/tracker_issue_test.gleam`. Keep the test data values unchanged, including issue identifier `ABC-123`, labels `bug` and `tests`, and blocker state `Done`.

7. Commit the tracker issue slice after tests pass. Suggested commit message: `refactor: move tracker issue domain types`.

8. Create `src/scherzo/config/types.gleam` and move all configuration types into it. Update `src/scherzo/config.gleam` first, then update consumers that reference `domain.TrackerConfig`, `domain.EffectiveConfig`, `domain.OrchestratorConfig`, `domain.ArtifactLimits`, or any other moved config type.

9. Run:

       grep -n -E "^pub type (TrackerConfig|PollingConfig|WorkspaceConfig|HooksConfig|UiRequestPolicy|AgentConfig|PiConfig|HandoffConfig|LinearContractConfig|LinearCommandConfig|RoutingConfig|DagHooksConfig|ArtifactLimits|OrchestratorConfig|EffectiveConfig)" src/scherzo/domain.gleam
       grep -R -n -E "domain\.(TrackerConfig|PollingConfig|WorkspaceConfig|HooksConfig|UiRequestPolicy|AgentConfig|PiConfig|HandoffConfig|LinearContractConfig|LinearCommandConfig|RoutingConfig|DagHooksConfig|ArtifactLimits|OrchestratorConfig|EffectiveConfig)" src test
       direnv exec . gleam test

   The first grep should print no matches, proving the moved configuration records are not duplicated in `src/scherzo/domain.gleam`. The second grep should print no matches, and the tests should exit successfully. Pay particular attention to existing tests named `test/config_test.gleam`, `test/orchestrator_config_test.gleam`, `test/runtime_bundle_test.gleam`, and `test/workflow_policy_test.gleam`.

10. Commit the configuration slice after tests pass. Suggested commit message: `refactor: move configuration domain types`.

11. Move `ResultArtifact` into `src/scherzo/result_artifact.gleam`, update any residual `src/scherzo/domain.gleam` field annotations to `result_artifact.ResultArtifact`, then run:

       grep -n "^pub type ResultArtifact" src/scherzo/domain.gleam
       grep -R -n "domain\.ResultArtifact" src test
       direnv exec . gleam test

   The first grep should print no matches, proving the moved record is not duplicated in `src/scherzo/domain.gleam`. The second grep should print no matches. Existing tests in `test/result_artifact_test.gleam`, `test/handoff_test.gleam`, and `test/linear_attachment_test.gleam` should pass as part of the full test command.

12. Create `src/scherzo/session/tokens.gleam`, move `TokenTotals` and `zero_token_totals` into it, and move the `default_token_totals_are_zero_test` assertion from `test/domain_test.gleam` into a new `test/session_tokens_test.gleam`.

13. Run:

       grep -n -E "^pub type TokenTotals|^pub fn zero_token_totals" src/scherzo/domain.gleam
       grep -R -n -E "domain\.(TokenTotals|zero_token_totals)" src test
       direnv exec . gleam test

   The first grep should print no matches, proving the moved token definitions are not duplicated in `src/scherzo/domain.gleam`. The second grep should print no matches, and the tests should exit successfully.

14. Create `src/scherzo/session/live.gleam`, move `LiveSession` into it, and update session, pi, terminal, control, and orchestrator consumers.

15. Run:

       grep -n "^pub type LiveSession" src/scherzo/domain.gleam
       grep -R -n "domain\.LiveSession" src test
       direnv exec . gleam test

   The first grep should print no matches, proving the moved live-session record is not duplicated in `src/scherzo/domain.gleam`. The second grep should print no matches. Existing tests in `test/session_event_test.gleam`, `test/session_hub_test.gleam`, `test/pi_client_test.gleam`, `test/pi_protocol_test.gleam`, `test/terminal_render_test.gleam`, and control protocol tests should pass as part of the full test command.

16. Add `RunAttempt` to `src/scherzo/agent/run_attempt.gleam`, update consumers to use `run_attempt.RunAttempt`, update any residual `src/scherzo/domain.gleam` field annotations to `run_attempt.RunAttempt`, and run:

       grep -n "^pub type RunAttempt" src/scherzo/domain.gleam
       grep -R -n "domain\.RunAttempt" src test
       direnv exec . gleam test

   The first two greps should print no matches, and the tests should exit successfully.

17. Create `src/scherzo/workspace/record.gleam`, move `WorkspaceRecord` into it, update consumers and any residual `src/scherzo/domain.gleam` field annotations, and run:

       grep -n "^pub type WorkspaceRecord" src/scherzo/domain.gleam
       grep -R -n "domain\.WorkspaceRecord" src test
       direnv exec . gleam test

   The first two greps should print no matches, and the tests should exit successfully.

18. Commit the artifact, session, run attempt, and workspace record slice after tests pass. Suggested commit message: `refactor: move session and artifact domain types`.

19. Create `src/scherzo/orchestrator/state.gleam` and move `RetryEntry`, `RunningEntry`, `IssueCounter`, `new_issue_counter`, `ParkReleasePolicy`, `ParkedEntry`, `InvalidWorkflowReport`, and `RuntimeState` into it.

20. Move `default_issue_counter_is_zero_test`, `parked_issue_records_release_policy_test`, and `runtime_state_holds_scheduler_collections_test` from `test/domain_test.gleam` into a new `test/orchestrator_state_test.gleam`. Keep the exact assertions and update module qualifiers.

21. Run:

       grep -n -E "^pub type (RetryEntry|RunningEntry|IssueCounter|ParkReleasePolicy|ParkedEntry|InvalidWorkflowReport|RuntimeState)|^pub fn new_issue_counter" src/scherzo/domain.gleam
       grep -R -n -E "domain\.(RetryEntry|RunningEntry|IssueCounter|new_issue_counter|ParkReleasePolicy|ExplicitUnparkOnly|AutoUnparkOnIssueChange|ParkedEntry|InvalidWorkflowReport|RuntimeState)" src test
       direnv exec . gleam test

   The first grep should print no matches, proving the moved orchestrator state definitions are not duplicated in `src/scherzo/domain.gleam`. The second grep should print no matches, and existing orchestrator tests should pass as part of the full test command.

22. Commit the orchestrator state slice after tests pass. Suggested commit message: `refactor: move orchestrator runtime state types`.

23. Move the tracker kind and issue state tests currently in `test/domain_test.gleam` into an owner-specific test file, such as `test/tracker_types_test.gleam`, because they already exercise `src/scherzo/tracker/kind.gleam` and `src/scherzo/tracker/state.gleam` directly rather than testing `domain.gleam`.

24. Delete `test/domain_test.gleam` once it has no remaining tests. Delete `src/scherzo/domain.gleam` once it has no remaining exported definitions and no imports.

25. Run final structural validation:

       grep -R -n "import scherzo/domain" src test
       grep -R -n "domain\." src test
       grep -R -n "src/scherzo/domain.gleam" src test gleam.toml

   All three greps should print no matches. Do not search `.` for the final path check; ignored files, build output, temporary review notes, and historical planning documents can mention the old path without keeping the module alive. The acceptance concern is source, tests, and project configuration.

26. Run final formatting and tests:

       direnv exec . gleam format --check src test
       direnv exec . gleam test

   Both commands must exit with status 0. If formatting fails, run `direnv exec . gleam format src test`, inspect the diff, and rerun the check and tests.

27. Update this ExecPlan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective sections with the final greps and test result summary.

28. Commit the final cleanup slice after all validation passes. Suggested commit message: `refactor: remove aggregate domain module`.

## Testing and Falsifiability

This is a refactor, so the primary falsifiable claim is behavior preservation. The claim is false if any public protocol test, config test, workflow test, tracker test, orchestrator test, session test, or handoff test fails after only moving type ownership.

Create or update tests along ownership boundaries instead of keeping `test/domain_test.gleam`:

- `test/tracker_issue_test.gleam` should construct a tracker issue with identifier `ABC-123`, labels `bug` and `tests`, and one blocker with state `Done`. It should assert the same field values that `issue_records_labels_and_blockers_test` currently asserts.
- `test/tracker_types_test.gleam` should contain the current tracker kind and issue state tests from `test/domain_test.gleam`, because those tests already exercise `src/scherzo/tracker/kind.gleam` and `src/scherzo/tracker/state.gleam`.
- `test/session_tokens_test.gleam` should call `session_tokens.zero_token_totals()` and assert that `input`, `output`, `cache_read`, `cache_write`, and `total` are all `0`.
- `test/orchestrator_state_test.gleam` should call `orchestrator_state.new_issue_counter()` and assert zero failure attempts and worker sessions. It should construct `orchestrator_state.ParkedEntry` with `orchestrator_state.AutoUnparkOnIssueChange("fingerprint")` and assert the release policy and park reason are preserved. It should construct `orchestrator_state.RuntimeState` with empty dictionaries and assert `poll_interval_ms == 30_000` and `max_concurrent_agents == 10`.

Focused test runs are useful after each slice, but they do not replace the full suite. The full falsifiability command is:

    direnv exec . gleam test

The expected result is exit status 0 with gleeunit reporting all tests passed. Record the exact pass count in this document when implementing, because the count may change before the plan is executed.

The final structural falsifiability checks are:

    grep -R -n "import scherzo/domain" src test
    grep -R -n "domain\." src test
    grep -R -n "src/scherzo/domain.gleam" src test gleam.toml

All three should print no matches at the end. If any command prints a match in `src`, `test`, or `gleam.toml`, the plan has not achieved its purpose.

Each slice also has a duplicate-definition check against `src/scherzo/domain.gleam`. These checks prove that moved nominal types were deleted from the residual module instead of being copied into both old and new modules. A duplicate-definition grep printing a match after its slice falsifies that slice even if the full test suite still compiles.

Protocol preservation is falsified by failures in existing protocol and rendering tests. The final validation must include the tests in `test/control_protocol_test.gleam`, `test/control_server_test.gleam`, `test/pi_protocol_test.gleam`, `test/pi_client_test.gleam`, `test/session_event_test.gleam`, `test/session_hub_test.gleam`, `test/terminal_render_test.gleam`, `test/linear_test.gleam`, `test/linear_contract_test.gleam`, `test/handoff_test.gleam`, `test/result_artifact_test.gleam`, `test/workflow_dag_test.gleam`, `test/workflow_run_test.gleam`, `test/workflow_scheduler_test.gleam`, `test/state_record_test.gleam`, `test/state_projection_test.gleam`, and `test/state_recovery_test.gleam`. Running the full suite is the easiest way to include them all.

## Validation and Acceptance

Implementation is accepted when all of the following are true from the repository root:

- `grep -R -n "import scherzo/domain" src test` prints no matches.
- `grep -R -n "domain\." src test` prints no matches.
- `src/scherzo/domain.gleam` no longer exists. A compatibility module or re-export is not accepted by this plan because `scherzo/domain` is treated as internal application code; a discovered external consumer requires a separate compatibility plan.
- Owner modules exist at the paths named in the Strategy Overview.
- The split tests exist and pass: `test/tracker_issue_test.gleam`, `test/tracker_types_test.gleam`, `test/session_tokens_test.gleam`, and `test/orchestrator_state_test.gleam`.
- `direnv exec . gleam format --check src test` exits with status 0.
- `direnv exec . gleam test` exits with status 0.
- `grep -R -n "src/scherzo/domain.gleam" src test gleam.toml` prints no matches.
- No YAML workflow grammar, config key, JSON protocol key, Linear payload shape, control protocol shape, pi protocol shape, hook name, or scheduler behavior changed as part of the refactor.

A reviewer should be able to inspect the final diff and see mostly moved type definitions plus import and qualifier updates. Any non-move logic change must have a Decision Log entry and a test explaining why it was necessary.

## Rollout, Recovery, and Idempotence

This refactor is internal to the Gleam codebase and should be rolled out as ordinary code commits. It does not require a data migration, daemon flag, config migration, or protocol version bump because it preserves runtime behavior and serialized shapes.

The recovery path is to revert the last green slice. Each milestone ends with a passing full test run and a logical commit, so rollback can be done by reverting the most recent slice rather than untangling a half-finished broad move. If a slice becomes tangled, stop, restore the last committed state, and redo the slice with a narrower type group.

The steps are mostly idempotent if repeated carefully. Re-running greps and tests is always safe. Re-running file moves is safe only after checking whether the destination module already contains the moved definitions. Do not leave duplicate type definitions with the same constructor names in two modules. During intermediate slices, the residual `src/scherzo/domain.gleam` module may import owner modules for field references in not-yet-moved records, but it must not define or re-export moved types.

If `direnv exec . <command>` fails because `.envrc` is blocked, treat that as environment setup rather than test failure. Inspect `.envrc`, run `direnv allow .` from the repository root, and retry the same command.

## Artifacts and Notes

The initial source-control check for this plan found a clean working copy:

    The working copy has no changes.

The initial source import audit found approximately forty source files importing `scherzo/domain`. The implementation audit must include both `src` and `test`, because tests are part of the migration checklist and may contain additional `domain` imports or qualifiers.

The initial plan authoring pass intentionally did not run broad validation. Full validation belongs to the implementation milestones, after code moves are made.

## Interfaces and Dependencies

Do not add package dependencies for this refactor. Use existing Gleam modules and the dependencies already declared in `gleam.toml`.

The new tracker issue module must expose:

    pub type BlockerRef {
      BlockerRef(id: Option(String), identifier: Option(String), state: Option(issue_state.IssueState))
    }

    pub type Issue {
      Issue(
        id: String,
        identifier: String,
        title: String,
        description: Option(String),
        priority: Option(Int),
        state: issue_state.IssueState,
        branch_name: Option(String),
        url: Option(String),
        labels: List(String),
        blocked_by: List(BlockerRef),
        created_at: Option(Time),
        updated_at: Option(Time),
      )
    }

The new config types module must expose the same configuration constructors and fields that currently live in `src/scherzo/domain.gleam`. Consumers should import it as:

    import scherzo/config/types as config_types

The new session tokens module must expose:

    pub type TokenTotals {
      TokenTotals(input: Int, output: Int, cache_read: Int, cache_write: Int, total: Int)
    }

    pub fn zero_token_totals() -> TokenTotals

The new session live module must expose `LiveSession` with the same fields currently in `src/scherzo/domain.gleam`, including `session_id`, `pi_rpc_pid`, last pi event fields, token counters, and `turn_count`.

The new orchestrator state module must expose the same constructors currently used for retry scheduling, running entries, issue counters, parked entries, invalid workflow reports, and runtime state. Consumers should import it as:

    import scherzo/orchestrator/state as orchestrator_state

The final codebase must not depend on `src/scherzo/domain.gleam` for any of these interfaces.

## Open Questions and Clarifications Needed

None.
