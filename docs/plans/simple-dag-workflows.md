# Support orchestrator config and simple DAG workflows

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo can run an implementation workflow that is more than one prompt. An operator can keep Linear, polling, workspace storage, handoff, and pi defaults in a Scherzo orchestrator config, then define checked-in workflows as small directed acyclic graphs: implement the issue, run tests, fan out independent code/security/performance reviews in parallel logical workspaces, fan the findings back into a fix step, and run final validation. Existing `WORKFLOW.md` prompt workflows continue to work, but new multi-step workflows live in YAML files with separate Markdown prompt templates.

The visible proof for this plan is a checked-in example under `examples/` that can be copied to `.scherzo/scherzo.yaml`. In that example, a `workflow:implementation` issue routes to `workflows/implementation.yaml`, Scherzo runs the `implement` step first, runs `code_review`, `security_review`, and `performance_review` concurrently because they use different logical workspace names, serializes every step that uses the same logical workspace name, and passes the review and command artifacts into an `apply_feedback` prompt. Migrating this repository's dogfood `.scherzo/workflows/research.md` to a repo-local `.scherzo/scherzo.yaml` is intentionally left as a follow-up rollout step after the new mode is proven.

## Problem Framing and Constraints

Today `WORKFLOW.md` is both runtime config and prompt template. `src/scherzo/workflow.gleam` returns `domain.WorkflowDefinition(config, prompt_template)`, `src/scherzo/config.gleam` resolves tracker/runtime settings from the Markdown front matter, and `src/scherzo/agent/runner.gleam` prepares one workspace and runs one pi prompt loop for the issue. This makes frontmatter-only Markdown files feel odd, and it cannot represent implementation workflows that need a write step followed by parallel independent reviews and a fan-in step.

The config split must be complete, not just a new daemon dispatch path. Once mode, daemon mode, Linear smoke checks, Linear contract checks, config reload, instance locking, and documentation must all load the same chosen path and distinguish legacy Markdown workflow files from new YAML orchestrator config files. Otherwise operators would be able to run a DAG in one mode and get stale legacy behavior in another.

Tracker details are not workflow logic. Linear endpoint, API key, project slug, active states, polling interval, handoff settings, and local control behavior are orchestrator configuration. A workflow should start from "Scherzo selected an issue; now what steps should run for that issue?" Pulling tracker config out of workflow files makes workflows reusable and keeps secrets away from workflow definitions.

Workspace materialization must stay hook-owned. This repository already uses hooks to create and validate workspaces, including dogfood jj workspace hooks through `scripts/scherzo-jj-workspace`. Scherzo should not hard-code workspace modes like `canonical_write` or `snapshot_readonly`. The DAG should declare logical workspace names such as `main`, `code-review`, and `security-review`; Scherzo should compute paths, serialize steps that use the same logical workspace name, and call hooks with enough environment context for the script to create, derive, validate, or clean up the actual filesystem workspace.

The first implementation should stay deliberately smaller than a full workflow engine. It must support static DAGs, agent steps, command steps, dependency edges, failure policy, step artifacts, simple artifact interpolation into downstream prompts, and same-workspace serialization. It should not add loops, dynamic branching, manual approval gates, cross-issue dependencies, retry policy per step, or built-in git/jj semantics.

## Strategy Overview

Introduce a new, opt-in project config file and a new workflow DAG file format while preserving legacy Markdown workflows. When Scherzo is started with a legacy `WORKFLOW.md`, the current parser and config resolver keep working and the legacy prompt compiles internally to a one-step workflow. When Scherzo is started with a new `.scherzo/scherzo.yaml`, the orchestrator config is resolved from that file and workflow routing loads one or more `.scherzo/workflows/*.yaml` DAGs.

The new config file owns tracker, polling, workspace root and workspace hooks, pi defaults, agent limits, handoff, Linear command transport, and workflow routing. To keep legacy compatibility low-risk, `domain.EffectiveConfig` remains the shared resolved runtime config for existing keys. New-mode-only routing, DAG workspace hooks, artifact limits, and loaded workflow DAGs live in a new orchestrator/bundle type returned by `runtime_bundle`, rather than being hidden in Markdown front matter or forced into the legacy single-worker shape. A workflow DAG file owns only an id, description, defaults, and steps. Step prompts are Markdown files referenced from the workflow file directory. Command steps run shell commands in the step workspace and capture bounded stdout, stderr diagnostics, and exit status as artifacts. Agent steps run pi with a rendered prompt in the step workspace and capture the final assistant-visible response as an artifact.

The DAG scheduler is pure and testable. It validates unique step ids, missing dependencies, cycles, invalid workspace names, invalid `from` references, and unknown step kinds. At runtime it starts all ready steps up to a workflow-level parallelism limit, but it never starts two steps using the same logical workspace name at the same time. Different logical workspace names may run concurrently when dependencies are satisfied. When a ready batch contains derived workspaces, the workflow runner prepares those derived workspace directories from their source workspace before starting any selected step that runs in that source workspace, so a copy hook does not race with a command mutating the source. A step with `on_failure: fail` aborts the workflow when it fails. A step with `on_failure: continue` records its failure artifact and still counts as complete for downstream dependencies.

Workspace hooks remain the extension point. A step declares either `workspace: main` or `workspace: { name: code-review, from: main }`. Scherzo computes a path under the issue run root, then calls the configured hook from the orchestrator config directory with environment variables such as `SCHERZO_CONFIG_DIR`, `SCHERZO_STEP_ID`, `SCHERZO_WORKSPACE_NAME`, `SCHERZO_WORKSPACE_PATH`, `SCHERZO_SOURCE_WORKSPACE_NAME`, and `SCHERZO_SOURCE_WORKSPACE_PATH`; the source variables are empty strings when the step has no `from`. A hook can use `jj workspace add`, `git worktree`, `rsync`, clone, containers, or any other strategy. Scherzo only relies on the hook returning success and leaving a usable directory at the requested path.

The daemon should own workflow scheduling, not individual agents. This keeps the graph visible to Scherzo, lets parallel agent steps have their own session ids and event streams, and prevents one hidden agent process from spawning opaque child agents. The daemon claims an issue once, creates a workflow-run state for that issue, starts ready command or agent steps, records artifacts, schedules dependents, and reports a single workflow success or failure at the end.

## Alternatives Considered

The simplest alternative is to keep using frontmatter Markdown and instruct one agent to implement, test, review itself, and apply fixes. That is rejected because it gives no real parallelism, no independent review perspectives, no scheduler-visible artifacts, and no workspace isolation.

Another alternative is to put the entire DAG in Markdown front matter. That is rejected because multi-step YAML front matter would be an unreadable blob, and a frontmatter-only `.md` file is exactly the odd convention this plan is trying to retire. Markdown remains the right format for prompts, not for the orchestration graph.

A third alternative is to add hard-coded workspace modes such as `canonical_write`, `canonical_read`, and `snapshot_readonly`. That is rejected because Scherzo already delegates workspace creation to hooks, and repositories may use different source-control systems and isolation strategies. Logical workspace names plus hook context are less prescriptive and easier to adapt.

A fourth alternative is to hide the whole DAG inside `src/scherzo/agent/runner.gleam`, making one worker process spawn all child steps internally. That is rejected because the daemon and control API would see only one session, parallel child agents would have confusing interleaved events, and operator commands would have no clear target. The daemon already owns session registration and worker lifecycle, so the workflow graph should be represented there.

A fifth alternative is to build a general-purpose workflow engine with conditional branches, retries per node, manual approval gates, persisted graph state, and artifact files. That is rejected for this plan because the immediate need is a simple static fan-out/fan-in DAG for implementation workflows. More expressive features can be added after this smaller model is proven.

## Risks and Countermeasures

The main compatibility risk is breaking existing `WORKFLOW.md` users. Countermeasure: legacy Markdown parsing remains in `src/scherzo/workflow.gleam`, legacy config resolution remains supported, the default CLI path remains `WORKFLOW.md` in this plan, and legacy workflows are adapted to a one-step DAG internally. Existing tests in `test/workflow_test.gleam`, `test/config_test.gleam`, `test/orchestrator_service_test.gleam`, and daemon tests must continue to pass.

The main config-loading risk is implementing YAML config only for once or daemon execution while smoke checks, contract checks, reload, or lock naming still call `workflow.load` and silently ignore the new file. Countermeasure: introduce one bundle-loading boundary early, use it from every CLI mode that needs runtime config, and add service tests that exercise `.scherzo/scherzo.yaml` in once mode, daemon startup, Linear smoke, and Linear contract check.

The main scheduler risk is a graph that deadlocks or starts steps out of order. Countermeasure: implement validation before runtime scheduling and test duplicate ids, missing dependencies, cycles, invalid workspace references, and fan-out/fan-in readiness in a new pure `test/workflow_dag_test.gleam` and `test/workflow_scheduler_test.gleam`.

The main workspace risk is deleting or corrupting the wrong directory, or racing a derived workspace copy against a step that is mutating the source workspace. Countermeasure: reuse the existing path containment checks from `src/scherzo/workspace.gleam`, sanitize issue identifiers and logical workspace names, put new DAG workspaces under an issue run root, require cleanup to stay inside `workspace.root`, and prepare derived workspaces from their source before starting any selected same-batch step that runs in that source workspace. Keep legacy workspace paths unchanged for legacy Markdown mode.

The main hook risk is not giving scripts enough context to create derived workspaces. Countermeasure: extend hook execution to pass explicit environment variables for issue, workflow, step, workspace, source workspace, run id, and attempt. Add tests that assert a hook receives those variables and can create a derived workspace by copying a marker from the source workspace.

The main observability risk is confusing operator sessions when parallel reviews run. Countermeasure: each agent step gets a distinct session id containing the issue identifier, workflow run id, and step id. Session summaries gain optional `workflow_id` and `step_id` fields, and `scherzoctl ps` renders those fields so the operator can identify the active step session.

The main control risk is routing mutating operator commands to the wrong parallel agent. Countermeasure: operator commands continue to target a concrete session id. The daemon stores command subjects per step worker session, not per issue only. A prompt sent to the `security_review` session goes only to that agent step. If a command targets a workflow-run session or a completed step, it is rejected as not found or not allowed.

The main handoff risk is preserving existing retry and completion semantics. Countermeasure: treat a successful completion of all required DAG steps as a successful worker run. The final issue state is still refreshed and included when available, but the DAG reaching its terminal successful state is what tells the orchestrator to report workflow success. Legacy single-step workflows keep their current active/terminal continuation behavior until they are explicitly migrated to DAG files.

The main fatal-failure risk is leaving sibling steps running after one required step has already failed, which can waste pi capacity or mutate a workspace after the workflow outcome is already known. Countermeasure: when a `fail` step fails, mark the workflow as failing, stop scheduling new steps, best-effort terminate active step workers through the same worker-monitor path used by daemon shutdown, run best-effort after-step hooks for any step that had started, then clean up the run root only after active step monitors have drained or the shutdown timeout expires.

The main output risk is feeding huge command logs into prompts or Linear comments. Countermeasure: cap command stdout and stderr diagnostics separately, record a `truncated` flag, and expose bounded artifact variables to templates. Default caps should be conservative, for example 20,000 characters per command stream and 8,000 characters per artifact field in prompt context unless the config explicitly increases them.

The main artifact privacy risk is capturing command output, hook diagnostics, or agent responses that contain the Linear API key or other resolved secrets, then feeding those values into prompts, event logs, or Linear comments. Countermeasure: reuse `log.redact` with `config.resolved_secrets(effective)` before persisting or exposing step artifacts, add artifact redaction tests with a fake secret, and document that command steps should not print secrets even though Scherzo also redacts known values defensively.

The main concurrency risk is one issue running many inner pi agents while `agent.max_concurrent_agents` currently counts issue workers rather than step workers. Countermeasure: add a workflow-level `max_parallel_steps` with a safe default such as 4, and document that `agent.max_concurrent_agents` limits concurrently dispatched issues while `workflow.max_parallel_steps` limits concurrent steps within one issue. A later plan can add a global pi-process semaphore if real usage shows that issue-level and workflow-level caps are insufficient.

## Progress

- [x] (2026-04-30 00:20Z) Drafted this plan after reading the current workflow parser, config resolver, workspace hooks, agent runner, daemon/service orchestration, template renderer, event hub, tests, examples, and documentation.
- [x] (2026-04-30 00:20Z) Verified baseline validation: `direnv exec . gleam format --check src test` exited successfully and `direnv exec . gleam test` reported `309 passed, no failures`.
- [x] (2026-04-30 00:45Z) Reviewed the plan for implementability gaps and tightened the config-loading boundary, orchestrator bundle shape, identifier/path validation, fatal-failure handling, artifact redaction, and workflow result aggregation.
- [x] (2026-04-30 02:05Z) Added `src/scherzo/workflow_dag.gleam`, `src/scherzo/runtime_bundle.gleam`, orchestrator config/domain types, parser validation tests, config split tests, and runtime bundle tests while keeping legacy Markdown mode green.
- [x] (2026-04-30 02:20Z) Added pure scheduler, artifact, and template-local support with tests for fan-out/fan-in readiness, same-workspace serialization, bounded/redacted artifacts, and downstream `steps.*` prompt variables.
- [x] (2026-04-30 02:35Z) Added port environment support, DAG hook environment support, `src/scherzo/workspace_run.gleam`, and tests proving hook cwd, `SCHERZO_*` variables, derived workspace source paths, and safe cleanup boundaries.
- [x] (2026-04-30 02:45Z) Added a command-step executor and a lower-level `runner.run_prompt_in_workspace` entry point for prepared workspaces while preserving existing legacy runner behavior.
- [x] (2026-04-30 03:35Z) Added `src/scherzo/workflow_run.gleam` and tests for fan-out/fan-in artifact interpolation, `on_failure: continue`, fatal failure, and cleanup.
- [x] (2026-04-30 04:05Z) Wired runtime bundle workflow routing into once mode, Linear smoke/contract/probe paths, and lock acquisition while preserving legacy Markdown dispatch validation.
- [x] (2026-04-30 04:25Z) Added an initial YAML daemon path that claims an issue once, routes by workflow label, runs the DAG workflow runner, reports one workflow-level success, and reloads YAML bundles.
- [x] (2026-04-30 04:45Z) Changed the workflow runner to execute each ready batch concurrently after preparing all selected workspaces, while applying completed artifacts back in DAG order for deterministic downstream prompts.
- [x] (2026-04-30 05:05Z) Added concrete YAML agent step sessions in daemon mode and routed operator prompt/stop/respond/abort commands to the command subject registered by an active agent step session.
- [x] (2026-04-30 10:10Z) Added fatal ready-step cancellation in the workflow runner: a `fail` step result terminates still-active sibling step workers, runs the failed step's after-step hook, and cleans up without waiting for blocked siblings.
- [x] (2026-04-30 10:20Z) Added daemon event-hub sessions for YAML command steps, so command-only workflows also expose concrete step sessions such as `<run-id>-final_test`.
- [x] (2026-04-30 10:30Z) Replaced the YAML workflow's use of the legacy `workers`/`WorkerHandle` map with a dedicated `YamlRunHandle`, while keeping concrete command and agent step sessions in the daemon event hub.
- [x] (2026-04-30 03:00Z) Added `examples/scherzo.yaml`, example YAML workflows and prompts, README guidance, `.scherzo/README.md` guidance, `docs/SYMPHONY_SPEC.md` notes, and local YAML ignore rules.
- [x] (2026-04-30 03:05Z) Ran validation after the foundation slice: `direnv exec . gleam format --check src test` exited 0 and `direnv exec . gleam test` reported `354 passed, no failures`.
- [x] (2026-04-30 04:30Z) Ran validation after once-mode and initial daemon integration: `direnv exec . gleam format --check src test` exited 0 and `direnv exec . gleam test` reported `368 passed, no failures`.
- [x] (2026-04-30 05:10Z) Ran validation after concurrent ready batches and YAML step-session routing: `direnv exec . gleam format --check src test` exited 0 and `direnv exec . gleam test` reported `373 passed, no failures`.
- [x] (2026-04-30 10:15Z) Ran validation after fatal sibling cancellation: `direnv exec . gleam format --check src test` exited 0 and `direnv exec . gleam test` reported `377 passed, no failures`.
- [x] (2026-04-30 10:25Z) Ran validation after adding YAML command-step sessions: `direnv exec . gleam format --check src test` exited 0 and `direnv exec . gleam test` reported `377 passed, no failures`.
- [x] (2026-04-30 10:35Z) Ran final validation after replacing the YAML legacy worker-map wrapper: `direnv exec . gleam format --check src test` exited 0 and `direnv exec . gleam test` reported `377 passed, no failures`.

## Surprises & Discoveries

- Observation: The foundational DAG/config/workspace/step-executor slice can be added without changing the daemon dispatch path.
  Evidence: After adding the new modules and tests, `direnv exec . gleam test` reported `354 passed, no failures`; legacy service, daemon, workflow, config, workspace, and agent tests still pass.

- Observation: Erlang ports support hook environment injection cleanly through the `{env, [...]}` option while preserving the existing stderr diagnostics wrapper.
  Evidence: `src/scherzo_port_ffi.erl` now exports `start_with_env/3`, and `test/workspace_run_test.gleam` proves hook scripts see `SCHERZO_CONFIG_DIR`, step/workspace/source variables, and cwd equal to the config directory.

- Observation: Once mode can now execute YAML DAGs end-to-end through the shared workflow runner, and daemon mode exposes concrete command and agent step sessions without storing YAML runs in the legacy `workers` map.
  Evidence: `test/orchestrator_service_test.gleam` covers YAML once-mode routing and command execution; `test/orchestrator_daemon_test.gleam` covers YAML daemon startup, routing, command execution, completed-state accounting, command and agent step session registration, operator prompt routing to the active step session, and cleanup of stale step command routes after step crashes.

- Observation: Ready batches can be run concurrently without making downstream artifact rendering nondeterministic.
  Evidence: `src/scherzo/workflow_run.gleam` now spawns each selected ready step after preparing the whole batch, collects step results, and applies them back in DAG order. `test/workflow_run_test.gleam` proves two blocked ready command steps both start before either is released, while the fan-in prompt still receives deterministic `steps.*` artifacts.

- Observation: The current `WorkflowDefinition` is only a raw YAML config node plus a trimmed prompt body.
  Evidence: `src/scherzo/domain.gleam` defines `WorkflowDefinition(config: yay.Node, prompt_template: String)`, and `src/scherzo/workflow.gleam` fills those fields from optional Markdown front matter and body text.

- Observation: Current config resolution is tightly coupled to the workflow front matter but already accepts a raw YAML node internally.
  Evidence: `src/scherzo/config.gleam` resolves tracker, polling, workspace, hooks, agent, pi, handoff, Linear contract, and Linear command config from `workflow.config`. This can be refactored to resolve the same keys from a standalone config YAML root.

- Observation: Workspace behavior is already hook-owned and path-safe.
  Evidence: `src/scherzo/workspace.gleam` computes a path under `workspace.root`, ensures containment, calls `hooks.after_create` and `hooks.before_run`, and uses `safe_cleanup` to prevent deleting outside the root.

- Observation: The current template renderer can already support artifact variables with a small additive API.
  Evidence: `src/scherzo/template.gleam` evaluates `context.locals` before built-in issue variables. A new `render_with_locals` can pass keys such as `steps.code_review.final_response` without changing the template grammar.

- Observation: Event sessions are already independent objects in the event hub, but daemon state currently maps one issue to one session id.
  Evidence: `src/scherzo/session/hub.gleam` can register many `SessionSummary` values, while `src/scherzo/orchestrator/daemon.gleam` stores `issue_sessions: Dict(String, String)` and `workers: Dict(String, WorkerHandle)` keyed by issue id.

## Decision Log

- Decision: Add `.scherzo/scherzo.yaml` as the new orchestrator config and keep `WORKFLOW.md` as legacy compatibility.
  Rationale: Pulling tracker and runtime settings out of workflow definitions fixes the frontmatter-only Markdown smell without breaking current users.
  Date: 2026-04-30

- Decision: Use YAML for workflow DAG files and Markdown for prompt templates.
  Rationale: YAML is a natural fit for a small graph of typed steps, while Markdown remains the natural fit for prose instructions given to agents.
  Date: 2026-04-30

- Decision: Declare logical workspace names, not workspace implementation modes.
  Rationale: The repository already uses hooks for workspace materialization, and Scherzo should not assume git clones, jj workspaces, snapshots, or read-only mounts.
  Date: 2026-04-30

- Decision: Serialize steps with the same logical workspace name by default and allow different logical workspace names to run in parallel.
  Rationale: This is the smallest understandable concurrency rule that enables parallel reviews while avoiding concurrent mutation of the same workspace.
  Date: 2026-04-30

- Decision: Let the daemon schedule DAG steps rather than hiding the graph inside the agent runner.
  Rationale: The daemon owns claims, event sessions, operator command routing, worker monitoring, and cleanup. Keeping the graph visible there avoids opaque child agents and enables step-specific sessions.
  Date: 2026-04-30

- Decision: Successful DAG completion is a workflow success even if the issue is still in an active Linear state.
  Rationale: A DAG workflow should not need the agent to mutate Linear state just to stop Scherzo from re-running the whole graph. State movement belongs to handoff policy or human operators.
  Date: 2026-04-30

- Decision: Keep `domain.EffectiveConfig` as the shared legacy/runtime config and add a separate orchestrator bundle for new-mode-only data.
  Rationale: Forcing routing, DAG hook names, and loaded workflow graphs into the existing single-worker config would create unnecessary constructor churn and blur the compatibility boundary. The bundle can carry `effective`, `routing`, `dag_hooks`, `artifact_limits`, and parsed DAGs while existing code keeps using `EffectiveConfig` until it intentionally opts into YAML mode.
  Date: 2026-04-30

- Decision: Use strict portable identifiers and reject escaping file paths in V1.
  Rationale: Step ids become template variable path segments, workspace names become filesystem path segments and hook inputs, and workflow/prompt paths are operator-controlled. Restricting ids to simple lowercase forms and rejecting absolute or `..`-escaping paths prevents ambiguous templates and path traversal bugs.
  Date: 2026-04-30

- Decision: A fatal step failure stops scheduling and best-effort terminates active sibling steps before workflow cleanup.
  Rationale: Continuing to run parallel reviews after the workflow is already failed wastes capacity and can produce writes after a failure decision. Waiting for monitors or a bounded shutdown timeout keeps cleanup from deleting workspaces out from under live processes.
  Date: 2026-04-30

- Decision: Build the workflow-level result artifact from the final completed step plus a bounded per-step summary.
  Rationale: Existing handoff code expects a single `runner.WorkerSuccess`, but DAG workflows produce many artifacts. Using the final terminal step as the main response and appending a bounded, redacted summary of all steps preserves current handoff surfaces without hiding earlier failures that used `on_failure: continue`.
  Date: 2026-04-30

- Decision: Land the parser/config/scheduler/artifact/workspace/step-executor foundation before changing daemon dispatch.
  Rationale: The current daemon has many existing control, Linear command, handoff, retry, and session tests. Keeping the new YAML DAG path additive first retired schema, path-safety, artifact, hook-environment, and command-execution risk while preserving the legacy one-worker path. The remaining daemon/once integration can now be implemented against tested pure modules instead of mixing graph validation with worker lifecycle refactoring.
  Date: 2026-04-30

- Decision: Wire YAML once mode and an initial YAML daemon path through a shared workflow-level runner before refactoring daemon state into per-step workers.
  Rationale: This proves routing, claim-once behavior, handoff-once behavior, cleanup, command steps, agent-step prompt rendering, and failure policy end-to-end without disrupting existing daemon control/session tests. A focused follow-up can now replace the workflow-level daemon worker with step-level sessions and operator command subjects.
  Date: 2026-04-30

- Decision: Run scheduler-selected ready batches concurrently inside the shared workflow runner, then apply their artifacts back in DAG order.
  Rationale: This gives YAML workflows real fan-out/fan-in execution in once mode and the current daemon path without making artifact interpolation order depend on process scheduling. Same-workspace serialization still comes from the pure scheduler.
  Date: 2026-04-30

- Decision: Add concrete daemon-visible agent step sessions before replacing the workflow-level wrapper worker.
  Rationale: Operators need session ids that identify active agent steps. Registering step sessions and routing commands to their command subjects removes the most confusing operator-control gap while keeping claim, handoff, and cleanup semantics stable until the remaining YAML run-handle refactor is done.
  Date: 2026-04-30

- Decision: Track YAML issue runs with a dedicated `YamlRunHandle` instead of the legacy `WorkerHandle` map, while keeping step execution inside the tested workflow runner.
  Rationale: The feature needs one Linear claim/handoff lifecycle per issue and concrete step sessions for observability and operator commands. Replacing the legacy worker map entry with a YAML-specific run handle removes the misleading single-agent `WorkerHandle` without duplicating the pure DAG scheduler inside the daemon actor.
  Date: 2026-04-30

## Outcomes & Retrospective

The foundation milestones are complete, and YAML DAGs now run end-to-end in once mode plus a daemon path. Scherzo has a typed DAG parser, orchestrator YAML config resolver, runtime bundle loader, workflow runner, prompt-local rendering, redacted step artifacts, pure scheduler, hook environment support, DAG workspace preparation, command-step execution, a prepared-workspace agent-runner entry point, examples, and documentation. Ready DAG batches now execute concurrently, fatal ready-step failures terminate active siblings, and daemon YAML command and agent steps now get concrete event-hub sessions; active agent step sessions can receive operator prompts through their own command subjects. Legacy Markdown behavior still passes the existing suite. YAML issue runs are now tracked separately from the legacy `WorkerHandle` map, so the implementation satisfies the plan's claim-once, handoff-once, cleanup-once, fan-out/fan-in, failure-policy, and concrete step-session goals.

## Context and Orientation

This repository is a Gleam/Erlang project. The main executable is in `src/scherzo/main.gleam`. It parses CLI modes and calls `src/scherzo/orchestrator/service.gleam`, which handles once mode, smoke checks, contract checks, pi probe mode, and daemon startup. The daemon actor lives in `src/scherzo/orchestrator/daemon.gleam`. Pure issue-dispatch state transitions live in `src/scherzo/orchestrator/core.gleam`.

Current workflow loading is in `src/scherzo/workflow.gleam`. A file without front matter is treated entirely as the prompt body. A file starting with `---\n` is split at the next `\n---\n`; the front matter is parsed with `yay`, and the body becomes the prompt. `test/workflow_test.gleam` covers missing files, no front matter, map front matter, invalid YAML, non-map YAML, missing closing delimiter, and explicit path selection.

Current runtime config resolution is in `src/scherzo/config.gleam`. It resolves `tracker`, `polling`, `workspace`, `hooks`, `agent`, `pi`, `handoff`, `linear_contract`, and `linear_commands` from the workflow front matter. `test/config_test.gleam` covers default values, tracker validation and environment resolution, relative path resolution, hooks, agent limits, pi validation, handoff parsing, Linear contract, and Linear command settings.

Current workspace preparation is in `src/scherzo/workspace.gleam`. It sanitizes issue identifiers, resolves an issue workspace path under `workspace.root`, ensures the directory exists, calls `hooks.after_create` when a workspace is created or partially populated, calls `hooks.before_run` before the agent runs, and has cleanup helpers that refuse to delete paths outside the workspace root. Hooks are executed by `src/scherzo/hooks.gleam`, which uses `src/scherzo/port.gleam` and `src/scherzo_port_ffi.erl` to run a shell command in a cwd with stderr redirected to a diagnostics file.

Current agent execution is in `src/scherzo/agent/runner.gleam`. `run_attempt` prepares the workspace, renders `workflow.prompt_template` through `src/scherzo/template.gleam`, optionally runs a pi compatibility probe, launches pi RPC, loops turns, handles operator commands, refreshes issue state through the tracker client after turns, calls `workspace.after_run`, and returns `runner.WorkerSuccess` or `runner.WorkerFailure`.

Current daemon execution treats one issue as one worker. `src/scherzo/orchestrator/daemon.gleam` claims an issue, registers one event session, spawns one worker, routes operator commands to that worker's command subject, handles worker updates, and reports success or failure. `WorkerHandle` is keyed by issue id. This needs to change for DAG workflows because one issue run can have multiple step workers, including parallel agent steps.

Current event storage is in `src/scherzo/session/hub.gleam` and `src/scherzo/session/event.gleam`. A `SessionSummary` has a session id, issue id, issue identifier, issue title, workspace path, optional pi session id, status, turn count, timestamps, and token totals. Multiple sessions are already supported by the hub; the daemon currently chooses to register one per issue worker.

Current prompt templating supports variables like `{{ issue.identifier }}`, `{% if issue.description %}`, and `{% for label in issue.labels %}`. Unknown variables and filters fail rendering. The `Context` type has a `locals` list, but the public `render` function currently does not accept arbitrary locals. This plan will add a public rendering function that does.

The project uses `yay` for YAML parsing, `simplifile` for filesystem operations, `gleam_otp` for actors, `gleam_erlang` for processes, `gleam_json` for JSON, and `gleeunit` for tests. The validation commands are run from the repository root with `direnv exec . gleam format --check src test` and `direnv exec . gleam test`.

## Preconditions and Verified Facts

The repository currently has `src/scherzo/workflow.gleam`, `src/scherzo/config.gleam`, `src/scherzo/workspace.gleam`, `src/scherzo/hooks.gleam`, `src/scherzo/agent/runner.gleam`, `src/scherzo/orchestrator/service.gleam`, `src/scherzo/orchestrator/core.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/template.gleam`, `src/scherzo/session/hub.gleam`, and `src/scherzo/session/event.gleam`.

The repository currently has tests for workflow parsing, config parsing, workspace behavior, agent runner behavior, orchestrator service behavior, orchestrator core behavior, daemon behavior, control commands, session events, Linear command transport, handoff, and result artifacts under `test/`.

The current checked-in dogfood workflow is `.scherzo/workflows/research.md`. It is a Markdown workflow with front matter containing tracker, polling, workspace, hooks, agent, pi, handoff, Linear contract, and Linear command config, followed by a research prompt body.

The current `.gitignore` ignores runtime `.scherzo/*` state while allowing `.scherzo/README.md` and `.scherzo/workflows/**` to be checked in. It ignores `.scherzo/workflows/*.local.md` but does not yet mention `.scherzo/scherzo.local.yaml` or local YAML variants.

Baseline validation was run from the repository root. `direnv exec . gleam format --check src test` exited successfully. `direnv exec . gleam test` reported `309 passed, no failures`.

## Scope Boundaries

In scope for this plan are a new orchestrator config file format, a workflow DAG file format, legacy Markdown compatibility, workflow routing from labels to DAG files, static DAG validation, command steps, agent steps, bounded step artifacts, prompt interpolation of prior step artifacts, logical workspace names, derived-workspace hook context, same-workspace serialization, parallel execution of independent step workspaces, daemon and once-mode integration, tests, examples, and documentation.

All new identifiers have fixed portable grammars. Workflow ids and workspace names use lowercase letters, digits, `_`, and `-`, must start with a lowercase letter or digit, and must not be empty. Step ids use lowercase letters, digits, and `_`, must start with a lowercase letter, and must not contain `-` so artifact variables such as `{{ steps.code_review.final_response }}` remain easy to write. Routing workflow file paths and prompt file paths must be relative paths, must not contain `..` segments, and after normalization must stay under the directory that owns them: routing paths under the config file directory, prompt paths under the workflow YAML file directory. V1 rejects absolute paths for these references even if the host OS would allow them.

Out of scope are loops, conditionals, dynamic graph construction, manual approval gates, cross-issue dependencies, persisted graph resume after VM crash, artifact files larger than bounded inline artifacts, built-in git or jj workspace implementations, read-only filesystem enforcement, multiple tracker kinds, multiple tracker instances in one daemon, a default-path flip from `WORKFLOW.md` to `.scherzo/scherzo.yaml`, and a global pi-process semaphore shared across all workflow runs.

Legacy Markdown workflows remain supported. Their workspace path layout remains the current `<workspace.root>/<issue-identifier>` layout, their hooks remain the current top-level `hooks.after_create`, `hooks.before_run`, `hooks.after_run`, and `hooks.before_remove`, and their active-state continuation behavior remains unchanged.

New DAG workflows use a different workspace layout under the configured root. A reasonable path convention is `<workspace.root>/<workflow-id>/<issue-identifier>/<workspace-name>`, with an issue run root at `<workspace.root>/<workflow-id>/<issue-identifier>`. The exact layout must be documented and covered by tests. Cleanup for a completed DAG workflow deletes the issue run root after running the configured remove hook.

## Milestones

Milestone 1 adds the schema and loaders without changing issue execution behavior. At the end of this milestone, Scherzo can parse `.scherzo/scherzo.yaml`, resolve the existing orchestrator config keys from that file, parse `.scherzo/workflows/*.yaml`, validate the DAG, adapt a legacy Markdown workflow to a one-step internal DAG, and use the same bundle-loading boundary from once, daemon startup, Linear smoke, and Linear contract-check modes. Existing issue execution still calls the legacy runner. This milestone proves the file-format split without touching daemon scheduling.

Milestone 2 adds pure DAG scheduling and artifact/template support. At the end of this milestone, tests can construct a DAG, mark steps running/succeeded/failed, observe which steps are ready, prove same-workspace serialization, and render a downstream prompt using variables from completed step artifacts. No pi or shell command needs to run yet.

Milestone 3 adds logical workspace preparation with hook context. At the end of this milestone, Scherzo can prepare a step workspace by name, prepare a derived workspace with a source workspace path, pass the expected `SCHERZO_*` environment variables into hooks, and clean up only inside the configured workspace root. This proves that hooks, not hard-coded workspace modes, own materialization.

Milestone 4 extracts reusable step executors. At the end of this milestone, a command step can run a shell command in a prepared workspace and return a bounded artifact, and an agent step can render a prompt with issue and step-artifact context, run pi in a prepared workspace, and return an agent artifact. Existing `runner.run_attempt` remains available and passes existing tests.

Milestone 5 integrates DAG workflow runs into once mode and daemon mode. At the end of this milestone, a candidate issue is routed to a workflow DAG, the daemon claims the issue once, starts ready steps, fans out independent review steps in parallel, fan-in steps see dependency artifacts, failures obey `on_failure`, handoff runs once for the whole workflow run, cleanup removes the issue run root, and operator commands route to concrete agent step sessions.

Milestone 6 updates documentation, examples, and migration guidance. At the end of this milestone, the README explains the config/workflow split, `.scherzo/README.md` documents the new convention, examples include a simple research workflow and implementation DAG, and the old `examples/WORKFLOW.md` is clearly labeled as legacy-compatible.

## Plan of Work

Add a new module `src/scherzo/workflow_dag.gleam` for the workflow graph model and parser. Define types for `WorkflowDag`, `WorkflowStep`, `StepKind`, `WorkspaceRef`, `FailurePolicy`, and validation diagnostics. The parser should accept a YAML root map with `version: 1`, `id`, optional `description`, optional `max_parallel_steps`, optional `defaults`, and `steps`. An agent step requires `prompt`. A command step requires `run`. `depends_on` defaults to `[]`. `workspace` defaults to `main`; it may be either a string or a map containing `name` and optional `from`. `on_failure` defaults to `fail` and may be `fail` or `continue`. Validate ids with stable diagnostic codes: workflow ids and workspace names match `^[a-z0-9][a-z0-9_-]*$`; step ids match `^[a-z][a-z0-9_]*$`; dependency ids must reference existing step ids; a `from` value must name a logical workspace produced by at least one transitive dependency of that step so the source workspace is prepared before the derived workspace starts; `max_parallel_steps` must be at least 1.

Add a new module `src/scherzo/runtime_bundle.gleam`. This module chooses between legacy Markdown mode and new orchestrator-config mode by selected path extension and parsed shape: `.md` paths are legacy Markdown workflows, `.yaml` and `.yml` paths are orchestrator config files, and other extensions fail with a clear startup error. In legacy mode, it calls `workflow.load`, `config.resolve`, and builds a one-step DAG with step id `main`, kind `agent`, inline prompt from the Markdown body, workspace `main`, legacy hook mapping, and no routing. In new mode, it reads the selected YAML config path, resolves orchestrator config, loads every workflow file in the routing map, resolves prompt file paths relative to each workflow YAML file, rejects absolute or escaping paths, and returns a bundle containing `domain.EffectiveConfig`, routing metadata, DAG hook config, artifact limits, resolved secrets, and parsed `WorkflowDag` values.

Refactor `src/scherzo/config.gleam` so config resolution can operate on a YAML root node and a base path. Keep the current `resolve(workflow, workflow_path)` and `resolve_with_env(workflow, workflow_path, env)` functions as compatibility wrappers. Add `resolve_root(root, config_path, env)` for the shared `domain.EffectiveConfig` keys, and add `resolve_orchestrator_root(root, config_path, env)` for YAML config mode. `resolve_orchestrator_root` returns a new `domain.OrchestratorConfig` containing `effective`, `routing`, `dag_hooks`, and `artifact_limits`. Both functions resolve relative paths from the config file directory. Existing tests should continue to call the compatibility functions, new shared-config tests should call `resolve_root`, and YAML-mode tests should call `resolve_orchestrator_root`.

Extend the config schema for new orchestrator configs with a `routing` section. The minimal shape is:

    version: 1
    tracker:
      kind: linear
      api_key: "$LINEAR_API_KEY"
      project_slug: "$LINEAR_PROJECT_SLUG"
    workspace:
      root: workspaces
      hooks:
        create: ../scripts/scherzo-workspace create
        before_step: ../scripts/scherzo-workspace before-step
        after_step: ../scripts/scherzo-workspace after-step
        remove: ../scripts/scherzo-workspace remove
    routing:
      workflow_label_prefix: "workflow:"
      require_exactly_one_workflow_label: true
      workflows:
        research: workflows/research.yaml
        implementation: workflows/implementation.yaml

Keep existing `linear_contract` support. For the first implementation, derive `linear_contract.workflow_labels` from `routing.workflows` when `routing.require_exactly_one_workflow_label` is true and `linear_contract.workflow_labels` is absent. If both are specified, normalize both lists with the configured workflow label prefix and fail config validation unless they contain exactly the same workflow names. This keeps Linear contract checks and runtime routing from disagreeing.

Add workspace hook types for new configs without removing old hook names. Define a new `domain.DagHooksConfig` that lives on the orchestrator bundle rather than on `domain.EffectiveConfig`. The new hook names are `create`, `before_step`, `after_step`, and `remove`, plus `timeout_ms`. DAG hooks run with cwd set to the orchestrator config directory, not the step workspace, because the `create` hook may need to create the step workspace before it exists; hooks receive workspace paths through `SCHERZO_*` environment variables. The loader should map legacy top-level hooks to equivalent behavior for a single `main` workspace only inside the legacy bundle adapter; old runner paths continue to use `domain.HooksConfig` unchanged.

Modify hook execution to support environment variables by adding `port.start_with_env(command, cwd, vars)` in `src/scherzo/port.gleam` and `src/scherzo_port_ffi.erl`. The Erlang implementation should pass `{env, [{Key, Value}, ...]}` to `open_port` while preserving the existing `/bin/bash -lc` wrapper and stderr diagnostics file. Do not implement V1 by prepending shell `export` statements; shell quoting would be a larger security surface and harder to test than the port environment option.

Add `src/scherzo/workspace_run.gleam` for DAG workspace layout and hook invocation. It should compute the issue run root, compute each logical workspace path, validate containment under the configured root, prepare directories, call the create and before-step hooks with context, call after-step best-effort after each step, and expose cleanup for the run root. It should reuse or mirror `workspace.sanitize` and containment logic. It must not change legacy `workspace.prepare` behavior. Derived workspace preparation must be callable separately from step process start so the workflow runner can copy all selected derived workspaces before launching a selected step that uses one of their source workspaces.

Add `src/scherzo/step_artifact.gleam` for normalized step artifacts. The type should represent success or failure, final assistant response, command exit code, stdout, stderr diagnostics, truncation flags, and a compact `summary_text` used in handoff. Artifact construction must accept a `List(String)` of resolved secrets and redact those secrets before storing values, exposing template locals, logging summaries, or building the workflow-level result artifact. Provide a function that converts a dictionary of completed artifacts into template locals with keys like `steps.implement.status`, `steps.test_after_implement.exit_code`, `steps.test_after_implement.stdout`, `steps.code_review.final_response`, and `steps.security_review.final_response`.

Extend `src/scherzo/template.gleam` with `render_with_locals(template, issue, attempt, locals)`. The existing `render(template, issue, attempt)` should call the new function with an empty locals list. Add tests showing that `{{ steps.code_review.final_response }}` renders when supplied as a local and that unknown step variables still fail when not supplied.

Add `src/scherzo/workflow_scheduler.gleam` for pure runtime scheduling. It should define step runtime states such as pending, running, succeeded, failed-continued, and failed-fatal. It should provide functions to initialize state, list ready steps, mark a step running, mark a step finished, detect workflow completion, and detect workflow failure. Same-workspace serialization belongs here: a ready step is startable only when no currently running step has the same workspace name. The scheduler also enforces `max_parallel_steps` by returning at most the number of additional starts allowed by currently running steps; after any fatal failure it returns no ready steps.

Add a command step executor, for example `src/scherzo/command_step.gleam`. It should run a shell command in a workspace path, capture stdout lines until the process exits, read diagnostics from the port, cap stdout and diagnostics, and return a step artifact with exit code. Nonzero exit is a failed artifact. Timeouts should terminate the process and return a failed artifact. Command timeout can default to the hook timeout for V1 unless a `timeout_ms` field is added to command steps.

Extract reusable agent-step execution from `src/scherzo/agent/runner.gleam`. Add a public function that accepts an already prepared workspace path and a fully rendered prompt, then runs the existing probe and pi prompt loop. Preserve `run_attempt` and `run_attempt_with_command_ready` as wrappers for legacy behavior. The lower-level function should not prepare or clean up the workspace; the DAG workspace runner handles those hooks. It may still use existing pi session loop, command handling, result artifact extraction, and token accounting, but it must not apply the legacy active-state continuation rule. A DAG agent step is one step execution; only the workflow scheduler decides what runs after it.

Refactor daemon state in `src/scherzo/orchestrator/daemon.gleam` so an issue can own a workflow run with multiple step workers. Add a `WorkflowRunHandle` keyed by issue id that stores workflow id, run id, issue, scheduler state, artifacts, prepared workspace paths, active step handles, and aggregate token totals. Add a separate dictionary for step workers keyed by session id or a composite key of issue id plus step id, so operator commands target concrete step sessions. Keep legacy worker dictionaries for legacy mode until the migration is complete, or adapt legacy single-step workflows to use the same workflow-run path.

Update daemon dispatch. When a candidate passes issue-level preconditions, select a workflow by routing. In routing-enabled config, classify labels using the configured prefix and workflow map. If no route is selected, use the default workflow if configured; otherwise reject and optionally use the existing invalid workflow triage path. Claim the issue once, initialize the workflow-run scheduler, prepare and start ready steps, and publish workflow lifecycle events.

Update step completion handling. When a step finishes, record its redacted artifact, run after-step hook best-effort, update aggregate tokens, mark the step succeeded or failed according to the artifact and failure policy, start newly ready steps, or finish the workflow run. On fatal workflow failure, mark the run as failing, return no more ready steps, best-effort terminate active command and agent step workers, wait for their monitors or a bounded shutdown timeout, report failure once, release claims according to existing policy, and schedule retry or park through the existing core logic. On workflow success, build a `runner.WorkerSuccess` from the aggregate tokens, final refreshed issue, final terminal step artifact, and bounded per-step summary, report success once, cleanup the issue run root, release claims, and do not schedule a continuation solely because the issue remains in an active state.

Update once mode in `src/scherzo/orchestrator/service.gleam` to use the same bundle loader and workflow-run executor. Once mode can execute the DAG synchronously by repeatedly starting ready steps and waiting for the ready batch to finish. It does not need to expose live control commands, but it must use the same validation, artifacts, workspace hooks, redaction, fatal-failure cancellation, and failure semantics as daemon mode. Update Linear smoke and Linear contract-check entry points to use the bundle loader for YAML config paths while preserving legacy Markdown behavior for Markdown paths.

Update event sessions. Agent steps register a `SessionSummary` with a session id exactly equal to `<run-id>:<step-id>`, where `<run-id>` keeps the existing issue identifier, timestamp, and sequence format from `make_run_id`. Add optional `workflow_id` and `step_id` fields to `SessionSummary`, update JSON protocol encoding and terminal rendering tests, and render the fields in `scherzoctl ps`. Command steps should publish lifecycle events to the workflow-run session if a parent session exists, or simply log structured events if parent sessions are deferred.

Update examples and docs. Add `examples/scherzo.yaml`, `examples/workflows/research.yaml`, `examples/workflows/implementation.yaml`, and prompt files under `examples/workflows/prompts/`. Update `README.md`, `.scherzo/README.md`, and `docs/SYMPHONY_SPEC.md` to explain the new split, state that `examples/scherzo.yaml` can be copied to `.scherzo/scherzo.yaml`, and label old `WORKFLOW.md` as legacy-compatible. Update `.gitignore` to ignore local YAML variants such as `.scherzo/scherzo.local.yaml` and `.scherzo/workflows/**/*.local.yaml`; do not add a repo-local `.scherzo/scherzo.yaml` until the separate dogfood migration.

## Concrete Steps

1. From the repository root, run `git status --short` and confirm there are no unrelated local changes. If there are unrelated changes, stop and ask before editing them.

2. Create `test/workflow_dag_test.gleam` with failing tests for parsing a minimal workflow DAG, rejecting duplicate step ids, rejecting missing dependencies, rejecting cycles, accepting `workspace: main`, accepting `workspace: { name: code-review, from: main }` when the step depends on a prior `main` workspace step, rejecting `from: main` when no transitive dependency produces `main`, rejecting an empty workspace name, rejecting step ids with `-`, rejecting workspace names with `/` or `..`, rejecting `max_parallel_steps: 0`, defaulting `depends_on` to `[]`, and defaulting `on_failure` to `fail`.

3. Create `src/scherzo/workflow_dag.gleam` with the DAG types and parser/validator needed to make the tests from step 2 pass. Use `yay.parse_string` for YAML parsing, return `error.WorkflowParseError` or a new workflow DAG validation error type with stable codes, and keep validation pure.

4. Run `direnv exec . gleam test`. The new workflow DAG tests should pass, and existing tests should still pass. If existing tests fail, fix the compatibility break before continuing.

5. Add tests in `test/config_test.gleam` or a new `test/orchestrator_config_test.gleam` for resolving config from a standalone YAML root and config file path. Cover `tracker.project_slug` environment resolution, relative `workspace.root` resolution from the config file directory, a `routing.workflows` map whose paths are resolved from the config file directory, derivation of `linear_contract.workflow_labels` when absent, and validation failure when explicitly configured workflow labels disagree with routing.

6. Refactor `src/scherzo/config.gleam` to expose `resolve_root(root, config_path, env)` and `resolve_orchestrator_root(root, config_path, env)` while keeping `resolve` and `resolve_with_env` as wrappers. Add `domain.OrchestratorConfig`, `domain.RoutingConfig`, `domain.DagHooksConfig`, and `domain.ArtifactLimits`, and parse enough routing and DAG hook data for the tests from step 5. Run `direnv exec . gleam test`.

7. Add `test/runtime_bundle_test.gleam` with one test that loads a legacy Markdown workflow and receives an effective config plus a one-step DAG, another test that loads a new `scherzo.yaml` pointing to a workflow YAML file and prompt Markdown files, and negative tests for an absolute prompt path and a `../` prompt path that escapes the workflow directory. Use files under `test/tmp/` and `simplifile.write` like existing service tests.

8. Create `src/scherzo/runtime_bundle.gleam` and implement the loader. It should choose legacy mode for Markdown paths, new config mode for YAML paths, reject unknown extensions with a clear startup error, resolve and contain workflow and prompt paths as described above, and preserve `workflow.choose_path(None) == "WORKFLOW.md"` for now. Run `direnv exec . gleam test`.

9. Add `test/template_test.gleam` cases for `template.render_with_locals`, including rendering `{{ steps.code_review.final_response }}` and failing for an unknown artifact variable. Implement `render_with_locals` in `src/scherzo/template.gleam`. Run `direnv exec . gleam test`.

10. Add `test/step_artifact_test.gleam` with artifact-to-template-local tests for agent success, command success, command failure, truncation flags, redaction of a fake resolved secret, and construction of a workflow-level summary from multiple artifacts. Implement `src/scherzo/step_artifact.gleam`. Run `direnv exec . gleam test`.

11. Add `test/workflow_scheduler_test.gleam` with pure scheduler tests. Cover initial ready roots, no ready dependent before dependency success, fan-out after `implement`, fan-in before `apply_feedback`, same-workspace serialization, different-workspace parallel readiness, `max_parallel_steps`, no new ready steps after a fatal failure, `on_failure: fail`, and `on_failure: continue`.

12. Create `src/scherzo/workflow_scheduler.gleam` and make the tests from step 11 pass. Keep the module independent of pi, ports, Linear, and filesystem code. Run `direnv exec . gleam test`.

13. Add hook environment support. Write tests in `test/hooks_test.gleam` or `test/workspace_run_test.gleam` that run a hook script and assert it can read `SCHERZO_CONFIG_DIR`, `SCHERZO_STEP_ID`, `SCHERZO_WORKSPACE_NAME`, `SCHERZO_WORKSPACE_PATH`, and source workspace variables, and that the hook cwd is the config directory rather than the step workspace. Implement `port.start_with_env` in `src/scherzo/port.gleam` and `src/scherzo_port_ffi.erl` using the Erlang port environment option, preserving stderr diagnostics redirection. Run `direnv exec . gleam test`.

14. Create `test/workspace_run_test.gleam` for DAG workspace layout. Test that an issue `ABC-123` and workflow `implementation` with workspace `main` gets a path under the configured root, that `code-review` gets a different path, that `from: main` passes the source path to the hook, and that cleanup rejects paths outside the root.

15. Create `src/scherzo/workspace_run.gleam` and implement logical workspace preparation and cleanup. Reuse existing sanitization and containment logic from `src/scherzo/workspace.gleam` where practical. Do not change legacy `workspace.prepare`. Run `direnv exec . gleam test`.

16. Add `test/command_step_test.gleam` with a command that prints stdout and exits 0, a command that writes stderr and exits nonzero, a command that times out, and a command that prints a fake secret that must be redacted in the returned artifact. Implement `src/scherzo/command_step.gleam` using `src/scherzo/port.gleam`. Cap captured output and set truncation flags. Run `direnv exec . gleam test`.

17. Add an agent-runner extraction test in `test/agent_runner_test.gleam`. Prepare a workspace manually, pass a rendered prompt to the new lower-level function, use the existing fake pi fixture, and assert that it returns a `WorkerSuccess` with a workspace path and result artifact without calling `workspace.prepare` and without requesting a legacy active-state continuation. Implement the lower-level function in `src/scherzo/agent/runner.gleam` while keeping `run_attempt` behavior unchanged. Run `direnv exec . gleam test`.

18. Add a small workflow-run executor module, for example `src/scherzo/workflow_run.gleam`, that can execute a DAG given function dependencies for command steps, agent steps, workspace preparation, and logging. Write tests with fake step executors first, proving fan-out/fan-in, failure policies, and ready-batch ordering where derived workspace preparation from `main` happens before a selected `main` command starts.

19. Integrate the real command step, real workspace run, and real agent step into `workflow_run.gleam`. Add tests that use fake pi for one agent step and fake command executors for command steps. Run `direnv exec . gleam test`.

20. Refactor `src/scherzo/orchestrator/service.gleam` once mode to use `runtime_bundle` and `workflow_run` for new YAML config mode while leaving legacy mode passing through the existing path until the new path is stable. Add or update service tests for a YAML config with a simple one-step DAG, a command-only DAG, YAML-backed Linear smoke, and YAML-backed Linear contract check. Run `direnv exec . gleam test`.

21. Refactor `src/scherzo/orchestrator/daemon.gleam` in small commits. First add data structures for workflow runs and step workers without changing dispatch. Compile and run tests.

22. Change daemon dispatch for new YAML config mode to create a workflow run instead of a legacy single worker. Register step sessions for agent steps, start ready steps, and handle step completions. Keep legacy dispatch behavior for Markdown workflows until equivalent tests exist. Run `direnv exec . gleam test`.

23. Add daemon tests in a new `test/orchestrator_daemon_workflow_run_test.gleam`. Cover routing by `workflow:implementation`, rejecting missing or unknown workflow labels, starting parallel review steps after implement, not starting two same-workspace steps concurrently, completing the workflow only after fan-in dependencies finish, and best-effort terminating active sibling steps when a required parallel step fails fatally.

24. Update operator command routing tests so commands target a step session id. Verify that a prompt sent to one active step session reaches that step worker and that a prompt to a completed step is rejected or not found. Run `direnv exec . gleam test`.

25. Update handoff and cleanup tests. Verify that a successful DAG reports success once with a result artifact built from the final terminal step plus a bounded per-step summary, a fatal step failure reports failure once, `on_failure: continue` makes the redacted failed step artifact available downstream, secrets do not appear in handoff text, and cleanup removes only the issue run root. Run `direnv exec . gleam test`.

26. Add examples under `examples/` for the new config and workflow DAG. Include an implementation DAG with `implement`, `test_after_implement`, `code_review`, `security_review`, `performance_review`, `apply_feedback`, and `final_test` steps. Include prompt files under `examples/workflows/prompts/`.

27. Update `README.md`, `.scherzo/README.md`, and `docs/SYMPHONY_SPEC.md`. Explain legacy `WORKFLOW.md`, new `.scherzo/scherzo.yaml`, workflow routing, DAG syntax, logical workspace names, same-workspace serialization, hook cwd and environment variables, artifacts available to prompts, and limitations.

28. Update `.gitignore` so local YAML variants remain ignored, including `.scherzo/scherzo.local.yaml` and `.scherzo/workflows/**/*.local.yaml`. Do not add a repo-local `.scherzo/scherzo.yaml` in this plan; document the pattern with `examples/scherzo.yaml` first and leave the dogfood config migration to a separate follow-up.

29. Run final validation from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

    Expected result: format check exits 0, and all tests pass. The exact test count may be higher than the current baseline of 309.

30. Commit the work in logical commits. Suggested commit groups are parser/config split, pure scheduler/artifacts/template locals, workspace hooks, step executors, daemon integration, and docs/examples. Each commit should pass `direnv exec . gleam test` before being created.

## Testing and Falsifiability

The parser work is falsified if a workflow YAML with duplicate ids, invalid id characters, missing dependencies, a `from` source workspace that is not produced by a transitive dependency, a zero parallelism limit, or a cycle loads successfully. Add tests that assert these cases fail with stable diagnostic codes. Also test that a valid implementation DAG parses into the exact step ids, dependencies, workspace names, source workspace names, maximum parallelism, and failure policies expected by the sample file.

The config split is falsified if a standalone `.scherzo/scherzo.yaml` cannot resolve the same tracker, polling, workspace, agent, pi, handoff, Linear contract, and Linear command defaults that current front matter resolves, or if Linear smoke and contract-check modes cannot load that YAML path. Add tests that use the same inputs in legacy front matter and standalone YAML and compare the resulting shared `EffectiveConfig` fields. Add separate tests for YAML-only routing, DAG hook config, artifact limits, and routing/Linear-contract agreement.

The legacy compatibility claim is falsified if any existing test fails or if `examples/WORKFLOW.md` no longer parses and runs as a one-step workflow. Keep the existing workflow and config tests unchanged where possible. Add one explicit adapter test proving that a Markdown body becomes an internal one-step agent DAG with step id `main`.

The scheduler claim is falsified if two steps with the same logical workspace can be ready/running at the same time, if independent workspaces do not become ready together after their dependency completes, if more than `max_parallel_steps` can start, or if new work is returned after a fatal failure. Pure scheduler tests should check these cases without filesystem, pi, or daemon dependencies.

The hook-owned workspace claim is falsified if Scherzo needs to know whether a workspace is a git clone, jj workspace, copy, or snapshot, or if a derived workspace hook can race with a concurrently started command in its source workspace. Workspace tests should use trivial shell hooks and marker files only. The hook context test should show that a derived workspace can be created by reading `SCHERZO_SOURCE_WORKSPACE_PATH`; no production source-control command should be required for tests. Workflow-run tests should assert that selected derived workspace preparations from `main` finish before a selected `main` command process starts.

The artifact claim is falsified if downstream prompts cannot access step outputs, if huge outputs are inserted uncapped, or if known resolved secrets appear in stored artifacts, template locals, workflow summaries, logs, or handoff text. Template and artifact tests should assert exact locals for agent response, command exit code, stdout, stderr, status, truncation, and redaction.

The command-step claim is falsified if stdout, stderr diagnostics, nonzero exit, or timeout cannot be represented as artifacts. Command step tests should cover all four paths with small shell snippets.

The agent-step claim is falsified if extracting a lower-level runner changes existing `runner.run_attempt` behavior. Existing agent runner tests must pass. Add one new lower-level test using `test/fixtures/fake_pi_rpc.sh` to prove a prepared workspace plus rendered prompt can run without calling `workspace.prepare` inside the lower-level function.

The daemon integration claim is falsified if a workflow with `implement -> [code_review, security_review, performance_review] -> apply_feedback` runs the reviews serially without same-workspace pressure, starts `apply_feedback` before all review artifacts exist, runs two `main` workspace steps at once, or leaves sibling workers running after a required step fails fatally. Daemon workflow-run tests should use fake step executors with controlled completion subjects so ordering, cancellation, and cleanup can be asserted deterministically.

The handoff/retry claim is falsified if a completed DAG is immediately scheduled as a continuation just because the issue state remains active. Add a daemon or core integration test that completes all DAG steps for an active issue and asserts no continuation retry is scheduled for that reason. Keep legacy tests proving old active-state continuation behavior for Markdown workflows.

Final validation is the repository-wide command:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

At plan drafting time, the second command reported `309 passed, no failures`. After implementation, the count should increase and there should still be no failures.

## Validation and Acceptance

A new YAML config mode is accepted when a test fixture with this shape loads successfully:

    version: 1
    tracker:
      kind: linear
      api_key: test-key
      project_slug: TEST
    workspace:
      root: workspaces
      hooks:
        create: |
          mkdir -p "$SCHERZO_WORKSPACE_PATH"
        before_step: |
          test -d "$SCHERZO_WORKSPACE_PATH"
    routing:
      workflow_label_prefix: "workflow:"
      require_exactly_one_workflow_label: true
      workflows:
        implementation: workflows/implementation.yaml
    artifact_limits:
      command_stream_max_chars: 20000
      template_field_max_chars: 8000

A YAML config path is also accepted only when the once-mode loader, daemon startup, Linear smoke command, and Linear contract-check command all resolve the same tracker and Linear contract settings from the YAML file. A fixture that configures both `routing.workflows.implementation` and `linear_contract.workflow_labels: [implementation]` must pass; a fixture that configures `routing.workflows.implementation` and `linear_contract.workflow_labels: [research]` must fail before dispatch.

A workflow DAG is accepted when this shape parses and validates:

    version: 1
    id: implementation
    max_parallel_steps: 4
    steps:
      - id: implement
        kind: agent
        prompt: prompts/implement.md
        workspace: main
      - id: test_after_implement
        kind: command
        depends_on: [implement]
        run: gleam test
        workspace: main
        on_failure: continue
      - id: code_review
        kind: agent
        depends_on: [implement]
        prompt: prompts/code-review.md
        workspace:
          name: code-review
          from: main
      - id: security_review
        kind: agent
        depends_on: [implement]
        prompt: prompts/security-review.md
        workspace:
          name: security-review
          from: main
      - id: performance_review
        kind: agent
        depends_on: [implement]
        prompt: prompts/performance-review.md
        workspace:
          name: performance-review
          from: main
      - id: apply_feedback
        kind: agent
        depends_on:
          - test_after_implement
          - code_review
          - security_review
          - performance_review
        prompt: prompts/apply-feedback.md
        workspace: main
      - id: final_test
        kind: command
        depends_on: [apply_feedback]
        run: gleam test
        workspace: main

The scheduler is accepted when `implement` is the only initially ready step, then `test_after_implement`, `code_review`, `security_review`, and `performance_review` become ready after `implement` succeeds, but `test_after_implement` cannot start while another `main` step is running. The workflow runner is accepted when it prepares the three review workspaces from `main` before launching `test_after_implement` in `main` from the same ready batch. The three review steps can run together because they use different logical workspace names, subject to `max_parallel_steps`; lowering `max_parallel_steps` to 2 should return only two total startable steps until one finishes. After a fatal step failure, no additional pending steps are returned as ready.

The artifact fan-in is accepted when `apply_feedback` can render a prompt containing:

    The code review said:
    {{ steps.code_review.final_response }}

    The security review said:
    {{ steps.security_review.final_response }}

    The first test command exited with {{ steps.test_after_implement.exit_code }}.

and the rendered prompt contains the actual bounded, redacted artifacts from those dependency steps. If a dependency artifact contains a configured fake secret such as `test-key`, the rendered prompt and workflow summary must contain `[REDACTED]` instead of the raw secret.

The daemon integration is accepted when a test candidate labeled `workflow:implementation` creates one workflow run, starts step sessions with identifiable step ids, runs review steps in parallel under controlled fake executors, records artifacts, runs fan-in, reports one success, and leaves no running state for the issue after completion. A companion failure test is accepted when one required parallel review fails, Scherzo stops scheduling, sends best-effort termination to active sibling step workers, reports one failure, and cleans up only after the active monitors drain or the bounded shutdown timeout expires.

The legacy mode is accepted when running the existing test suite still passes and a legacy Markdown workflow can still be started through `service.run_once_with_dependencies(Some(path), deps)`.

## Rollout, Recovery, and Idempotence

This change is opt-in. The default path remains `WORKFLOW.md`, and existing Markdown workflows continue to work. Operators can adopt the new model by explicitly starting Scherzo with `.scherzo/scherzo.yaml` or another YAML config path after tests and examples are in place.

The first rollout should use examples and test fixtures, not immediately migrate the dogfood `.scherzo/workflows/research.md`. After the new mode is stable, create a separate migration commit that adds `.scherzo/scherzo.yaml` and converts or parallels the research workflow as `.scherzo/workflows/research.yaml` plus prompt Markdown. Keep the old Markdown workflow for one release window if practical.

If the new DAG path fails in production, operators can stop Scherzo and restart with the old Markdown workflow path. Because legacy parsing and runtime behavior are preserved, rollback is configuration-only as long as the old workflow file remains.

Workspace cleanup must be idempotent. A create hook may be called for an existing logical workspace and should validate or repair it. A remove hook may be called after a partial failure and should tolerate missing child paths. Scherzo cleanup must refuse to delete anything outside the configured workspace root even if a hook or config produces a surprising path.

Partial implementation is safe if milestones are committed in order. Parser and scheduler modules are additive. Workspace hook changes are covered by tests before daemon integration. Daemon integration should be guarded by the new YAML config mode so legacy workflows are not forced onto the new scheduler until tests prove parity.

## Artifacts and Notes

Baseline validation transcript from plan drafting:

    direnv exec . gleam format --check src test
    # exited 0

    direnv exec . gleam test
    # ...
    # 309 passed, no failures

Current legacy workflow object:

    pub type WorkflowDefinition {
      WorkflowDefinition(config: yay.Node, prompt_template: String)
    }

Current legacy hook names:

    hooks:
      after_create: ...
      before_run: ...
      after_run: ...
      before_remove: ...
      timeout_ms: 60000

New hook environment should include at least:

    SCHERZO_CONFIG_DIR=<absolute directory containing scherzo.yaml>
    SCHERZO_WORKFLOW_ID=implementation
    SCHERZO_RUN_ID=<stable run id>
    SCHERZO_ISSUE_ID=<tracker issue id>
    SCHERZO_ISSUE_IDENTIFIER=LIV-123
    SCHERZO_STEP_ID=code_review
    SCHERZO_WORKSPACE_ROOT=<resolved workspace root>
    SCHERZO_WORKSPACE_NAME=code-review
    SCHERZO_WORKSPACE_PATH=<resolved workspace path>
    SCHERZO_SOURCE_WORKSPACE_NAME=main
    SCHERZO_SOURCE_WORKSPACE_PATH=<resolved source workspace path>

For a step without `from`, always set `SCHERZO_SOURCE_WORKSPACE_NAME` and `SCHERZO_SOURCE_WORKSPACE_PATH` to empty strings. Tests should assert this behavior so hook scripts can branch on an empty variable instead of checking for unset environment keys.

Identifier and path rules for V1:

    workflow id / workspace name: ^[a-z0-9][a-z0-9_-]*$
    step id: ^[a-z][a-z0-9_]*$
    routing workflow path: relative to the config file directory, no absolute path, no .. segment
    prompt path: relative to the workflow YAML file directory, no absolute path, no .. segment
    agent step session id: <run-id>:<step-id>
    DAG hook cwd: SCHERZO_CONFIG_DIR

Workflow result aggregation for handoff should use the final terminal step artifact as the primary `ResultArtifact.final_response`. If the final step is a command step, use its bounded stdout when nonempty, otherwise its bounded stderr diagnostics. Append or include a bounded summary with one line per step containing step id, status, exit code when present, and truncation markers. Redact all resolved secrets before producing this aggregate text.

## Interfaces and Dependencies

In `src/scherzo/workflow_dag.gleam`, define a workflow graph API equivalent to:

    pub type WorkflowDag {
      WorkflowDag(
        id: String,
        description: Option(String),
        max_parallel_steps: Int,
        steps: List(WorkflowStep),
      )
    }

    pub type WorkflowStep {
      WorkflowStep(
        id: String,
        kind: StepKind,
        depends_on: List(String),
        workspace: WorkspaceRef,
        on_failure: FailurePolicy,
      )
    }

    pub type StepKind {
      AgentStep(prompt: PromptRef)
      CommandStep(run: String, timeout_ms: Option(Int))
    }

    pub type PromptRef {
      PromptFile(String)
      PromptInline(String)
    }

    pub type WorkspaceRef {
      WorkspaceRef(name: String, from: Option(String))
    }

    pub type FailurePolicy {
      FailWorkflow
      ContinueWorkflow
    }

The exact module may keep these types local rather than putting them in `domain.gleam`, but they must be importable by tests, the bundle loader, scheduler, and workflow runner.

In `src/scherzo/domain.gleam`, add new-mode config shapes equivalent to:

    pub type OrchestratorConfig {
      OrchestratorConfig(
        effective: EffectiveConfig,
        config_dir: String,
        routing: RoutingConfig,
        dag_hooks: DagHooksConfig,
        artifact_limits: ArtifactLimits,
      )
    }

    pub type RoutingConfig {
      RoutingConfig(
        workflow_label_prefix: String,
        require_exactly_one_workflow_label: Bool,
        default_workflow: Option(String),
        workflows: Dict(String, String),
      )
    }

    pub type DagHooksConfig {
      DagHooksConfig(
        create: Option(String),
        before_step: Option(String),
        after_step: Option(String),
        remove: Option(String),
        timeout_ms: Int,
      )
    }

    pub type ArtifactLimits {
      ArtifactLimits(
        command_stream_max_chars: Int,
        template_field_max_chars: Int,
        workflow_summary_max_chars: Int,
      )
    }

In `src/scherzo/config.gleam`, add:

    pub fn resolve_root(
      root: yay.Node,
      config_path: String,
      env: Env,
    ) -> Result(domain.EffectiveConfig, error.ConfigError)

    pub fn resolve_orchestrator_root(
      root: yay.Node,
      config_path: String,
      env: Env,
    ) -> Result(domain.OrchestratorConfig, error.ConfigError)

Keep the existing public functions:

    pub fn resolve(
      workflow: domain.WorkflowDefinition,
      workflow_path: String,
    ) -> Result(domain.EffectiveConfig, error.ConfigError)

    pub fn resolve_with_env(
      workflow: domain.WorkflowDefinition,
      workflow_path: String,
      env: Env,
    ) -> Result(domain.EffectiveConfig, error.ConfigError)

In `src/scherzo/template.gleam`, add:

    pub fn render_with_locals(
      template: String,
      issue: domain.Issue,
      attempt: Option(Int),
      locals: List(#(String, Value)),
    ) -> Result(String, error.TemplateError)

In `src/scherzo/port.gleam`, add:

    pub fn start_with_env(
      command: String,
      cwd: String,
      env: List(#(String, String)),
    ) -> Result(Process, PortError)

The Erlang implementation in `src/scherzo_port_ffi.erl` should pass environment variables to the opened port without removing the existing stderr redirection behavior.

In `src/scherzo/workspace_run.gleam`, expose functions equivalent to:

    pub fn prepare_step(
      issue: domain.Issue,
      workflow_id: String,
      run_id: String,
      step_id: String,
      workspace: workflow_dag.WorkspaceRef,
      orchestrator: domain.OrchestratorConfig,
      known_workspaces: Dict(String, PreparedStepWorkspace),
    ) -> Result(PreparedStepWorkspace, PrepareError)

    pub fn after_step(
      prepared: PreparedStepWorkspace,
      orchestrator: domain.OrchestratorConfig,
    ) -> Nil

    pub fn cleanup_run(
      run_root: String,
      orchestrator: domain.OrchestratorConfig,
    ) -> Result(Nil, error.WorkspaceError)

In `src/scherzo/workflow_scheduler.gleam`, expose pure functions equivalent to:

    pub fn init(dag: workflow_dag.WorkflowDag) -> SchedulerState
    pub fn ready_steps(dag: workflow_dag.WorkflowDag, state: SchedulerState) -> List(workflow_dag.WorkflowStep)
    pub fn mark_running(state: SchedulerState, step_id: String) -> SchedulerState
    pub fn mark_finished(state: SchedulerState, step_id: String, artifact: step_artifact.StepArtifact) -> SchedulerState
    pub fn mark_cancelling(state: SchedulerState) -> SchedulerState
    pub fn outcome(dag: workflow_dag.WorkflowDag, state: SchedulerState) -> WorkflowOutcome

In `src/scherzo/step_artifact.gleam`, expose functions equivalent to:

    pub fn from_agent_success(
      step_id: String,
      success: runner.WorkerSuccess,
      secrets: List(String),
      limits: domain.ArtifactLimits,
    ) -> StepArtifact

    pub fn from_command_result(
      step_id: String,
      exit_code: Int,
      stdout: String,
      stderr: String,
      timed_out: Bool,
      secrets: List(String),
      limits: domain.ArtifactLimits,
    ) -> StepArtifact

    pub fn to_template_locals(artifacts: Dict(String, StepArtifact)) -> List(#(String, template.Value))

    pub fn workflow_result_artifact(
      dag: workflow_dag.WorkflowDag,
      artifacts: Dict(String, StepArtifact),
      limits: domain.ArtifactLimits,
    ) -> domain.ResultArtifact

In `src/scherzo/agent/runner.gleam`, add a lower-level function for agent steps while preserving current wrappers. The shape can be adjusted to fit existing command handling, but it must accept a prepared workspace path and rendered prompt rather than a `WorkflowDefinition`:

    pub fn run_prompt_in_workspace(
      issue: domain.Issue,
      prompt: String,
      config: domain.EffectiveConfig,
      tracker_client: tracker.Client,
      emit_update: fn(String, PiUpdate) -> Nil,
      command_subject: process.Subject(worker_command.Command),
      on_command_ready: fn() -> Nil,
      workspace_path: String,
    ) -> Result(WorkerSuccess, WorkerFailure)

In `src/scherzo/orchestrator/daemon.gleam`, add workflow-run and step-worker state rather than overloading the existing one-worker-per-issue `WorkerHandle`. The exact names can change, but the state must be able to answer: which workflow is running for this issue, which steps are pending/running/completed, which artifacts are available, which logical workspace names are currently running, which session id belongs to each agent step, and which command subject handles operator commands for that step.
