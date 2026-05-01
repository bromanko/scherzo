# Decompose the daemon into focused orchestration subsystems

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo daemon mode should be easier and safer to change because the top-level daemon actor in `src/scherzo/orchestrator/daemon.gleam` will be orchestration glue instead of the implementation home for polling, retry timers, worker bookkeeping, workflow reloads, event publishing, Linear command routing, control commands, and side-effect execution. Operators should not see a feature change: the same daemon CLI, local control API, Linear command behavior, EventHub stream, worker lifecycle, retry policy, and workflow reload behavior must continue to work.

The visible proof is twofold. First, behavior stays green: from the repository root, `direnv exec . gleam test` passes after the repository `.envrc` is allowed; if direnv is unavailable in a disposable workspace, plain `gleam test` may be used only as a fallback when Gleam is already available. Daemon control tests still exercise the same public API, and targeted new tests prove side-effect worker crashes no longer stall the daemon. Second, the structure changes in an observable way: `src/scherzo/orchestrator/daemon.gleam` no longer owns the side-effect queue fields, side-effect spawning code, workflow reload implementation, event payload classification helpers, retry timer bookkeeping, or worker registry dictionaries directly. A reader adding a future daemon feature should be able to locate the relevant subsystem module without editing thousands of lines of unrelated actor code.

## Problem Framing and Constraints

The current daemon actor is a god object: one file, `src/scherzo/orchestrator/daemon.gleam`, is 3,991 lines in the current review workspace and owns nearly every runtime concern. It defines the actor `Message` type, worker handles, YAML step-session mappings, timer handles, control server handles, pending claims, side-effect variants, side-effect result variants, runtime dependencies, and one broad `State` record. The same file starts the control server, schedules poll ticks, reloads workflows, fetches tracker data, fetches Linear commands, dispatches workers, routes operator commands, publishes EventHub events, runs YAML workflow steps, schedules retries, performs handoff reporting, reports invalid workflow candidates, cleans workspaces, and shuts everything down.

This hurts operators indirectly. When unrelated behavior shares one actor and one state record, small changes increase the chance of stale state, missed cleanup, blocked command handling, and lifecycle bugs. The most concrete current safety risk is the side-effect queue: `src/scherzo/orchestrator/daemon.gleam` tracks `side_effects_in_flight` and `side_effect_queue`, then starts work with `process.spawn_unlinked`. If one side-effect process crashes before it sends `SideEffectFinished`, the daemon never decrements its in-flight count for that process. After enough crashes, the queue can stop draining even though the daemon actor itself is still alive.

This plan must preserve Scherzo's current Erlang-target Gleam architecture. It must not turn this refactor into a full OTP application rewrite, a durable crash-recovery project, a new workflow engine, or a change to user-visible daemon semantics. The daemon may remain the single top-level actor that receives public control messages; the important change is that it delegates coherent subsystems to focused modules and one focused side-effect runner actor.

## Strategy Overview

Use a strangler refactor. Add focused modules around one concern at a time, route the existing daemon through the new module, run the full suite, then delete the old daemon-local implementation for that concern. Start with the side-effect runner because it has a correctness risk, not just a taste issue. Then extract low-risk modules whose interfaces are clear: workflow reload, event publishing, worker registry, poll/retry scheduling, and control command handling.

The top-level daemon actor should remain responsible for public compatibility: `daemon.start`, `daemon.shutdown`, `daemon.get_snapshot`, and `daemon.apply_operator_command` continue to exist. Existing tests that construct `daemon.RuntimeDependencies`, use `daemon.TestTimer`, or return `daemon.NoControlServer` should either keep working unchanged or be updated in the same commit that deliberately moves those public helper types. Prefer keeping these compatibility types in `daemon.gleam` until the final cleanup, because this plan is about decomposing internals, not forcing a broad test API migration.

The new structure should be proportionate. Only the side-effect subsystem needs its own actor because it owns concurrent work and monitors. The worker registry, workflow reloader, event publisher, and scheduler can start as ordinary modules with explicit state and functions. If those modules later need actors, the boundaries will already be clear.

## Alternatives Considered

The simplest option is to leave the daemon as one file and add comments or region markers. That does not solve the side-effect crash stall, does not reduce the state surface touched by each feature, and still leaves every daemon change competing inside one 3,991-line module.

Another option is a full rewrite into a supervision tree with many OTP actors at once. That may eventually be attractive, but it is too risky for this problem. The existing daemon has broad behavior and a large test suite; a big-bang actor rewrite would make regressions hard to isolate. This plan introduces only one new actor where concurrency ownership is needed now.

A third option is to extract only pure helper functions while leaving the broad state record and side-effect queue in the daemon. That would reduce line count slightly but leave the architectural weakness intact. This plan moves ownership of state subsets, not just syntax.

A fourth option is to merge more behavior into `src/scherzo/orchestrator/core.gleam`. That module already owns pure runtime transitions such as worker success/failure, retry entries, reconciliation, parking, and invalid-workflow reporting. It should remain pure domain logic. Runtime processes, timers, monitors, file reloads, control command routing, and EventHub publishing should live in separate orchestration modules instead of bloating `core.gleam`.

## Risks and Countermeasures

The largest regression risk is changing lifecycle ordering while moving code. Countermeasure: each milestone is behavior-preserving except the side-effect crash fix, and each milestone ends with the validation command sequence defined in Validation and Acceptance. Existing daemon tests in `test/orchestrator_daemon_test.gleam`, `test/orchestrator_daemon_control_test.gleam`, `test/orchestrator_daemon_session_event_test.gleam`, `test/orchestrator_daemon_linear_command_test.gleam`, and `test/orchestrator_service_lifecycle_test.gleam` remain the primary parity suite.

The side-effect runner can accidentally deliver duplicate completions if a worker sends its result and then the monitor down message is also processed. Countermeasure: give every side effect a numeric id, keep an in-flight map by monitor and id, demonitor on normal finish, ignore stale monitor downs, and add a unit test that a successful effect produces exactly one completion.

The side-effect runner can hide crashes by only freeing queue capacity. Countermeasure: it must notify the daemon with `effect_runner.Crashed(id, effect, reason)`. The daemon must log `side_effect_crashed` and synthesize the same kind of failure result that the old handler already knows how to process, so poll and retry phases finish instead of staying in flight.

Extracted modules can create import cycles. Countermeasure: lower-level modules must not import `scherzo/orchestrator/daemon`. If a module needs to notify the daemon, it accepts a callback function such as `fn(effect_runner.Completion) -> Nil`. If a scheduler needs timers, it is generic over the timer handle type or accepts timer callbacks supplied by the daemon.

The public test helper API can churn. Countermeasure: keep `RuntimeDependencies`, `TimerHandle`, and `ControlServerHandle` in `daemon.gleam` during the main extraction. Move them only if the final cleanup proves it is worth the compatibility cost, and update all references in the same commit.

The final daemon can remain too large if extractions stop halfway. Countermeasure: final acceptance includes structural checks: `grep -E -n "side_effects_in_flight|side_effect_queue|fn run_side_effect|fn spawn_side_effect" src/scherzo/orchestrator/daemon.gleam` should print no matches, `grep -E -n "fn update_payload|fn kind_for_update|fn publish_worker_update" src/scherzo/orchestrator/daemon.gleam` should print no matches, and `wc -l src/scherzo/orchestrator/daemon.gleam` should show a substantial reduction from the verified current baseline of 3,991 lines. The exact final count is less important than the ownership boundaries, but a daemon still above roughly 2,500 lines should trigger a review before calling this complete.

## Progress

- [x] (2026-04-30 10:47Z) Recorded the initial authoring baseline with `direnv exec . gleam test`; at that time it passed with `377 passed, no failures`.
- [x] (2026-04-30 11:00Z) Drafted this plan against the current tree after reading `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/core.gleam`, `src/scherzo/orchestrator/service.gleam`, `src/scherzo/control/linear_transport.gleam`, `src/scherzo/tracker.gleam`, and `src/scherzo/error.gleam`.
- [x] (2026-04-30 23:14Z) Reviewed and corrected the plan against the current workspace: after reviewing `.envrc` and running `direnv allow .`, `direnv exec . gleam test` passes with `376 passed, no failures`, `daemon.gleam` is 3,991 lines, and no `.git` or `.jj` metadata is present in this checkout.
- [x] (2026-04-30 16:23Z) Milestone 0: added characterization coverage for the side-effect crash stall; the daemon integration test failed against the old unmonitored side-effect queue as expected because no `side_effect_crashed` log was emitted.
- [x] (2026-04-30 16:26Z) Milestone 1: introduced `src/scherzo/orchestrator/effect_runner.gleam`, added `test/orchestrator_effect_runner_test.gleam`, moved side-effect variants and result handling out of the daemon, removed daemon-local `side_effects_in_flight`, `side_effect_queue`, `spawn_side_effect`, and `run_side_effect`, and reached a green checkpoint with `379 passed, no failures`.
- [x] (2026-04-30 16:30Z) Milestone 2: introduced `src/scherzo/orchestrator/workflow_reloader.gleam`, added `test/orchestrator_workflow_reloader_test.gleam`, replaced daemon workflow path/content/bundle/reload/effective/secrets fields with `workflow_reloader.State`, and reached a green checkpoint with `382 passed, no failures`.
- [x] (2026-04-30 16:32Z) Milestone 3: introduced `src/scherzo/orchestrator/event_publisher.gleam`, added `test/orchestrator_event_publisher_test.gleam`, removed daemon-local event payload classification helpers, and reached a green checkpoint with `387 passed, no failures`.
- [x] (2026-04-30 17:12Z) Milestone 4: introduced `src/scherzo/orchestrator/worker_registry.gleam`, added `test/orchestrator_worker_registry_test.gleam`, replaced the daemon's worker, monitor, issue-session, YAML step-session, stopped-run, step-command-route, and session-sequence fields with `worker_registry.Registry`, and reached a green checkpoint with `393 passed, no failures`.
- [x] (2026-04-30 17:22Z) Milestone 5: introduced `src/scherzo/orchestrator/poll_scheduler.gleam` and `src/scherzo/orchestrator/retry_scheduler.gleam`, added targeted scheduler tests, replaced the daemon's poll generation/in-flight/timer and retry timer/refresh dictionaries with scheduler state, and reached a green checkpoint with `401 passed, no failures`.
- [x] (2026-04-30 18:20Z) Milestone 6: introduced `src/scherzo/orchestrator/control_command_handler.gleam`, added `test/orchestrator_control_command_handler_test.gleam`, moved the operator command decision tree, prompt/UI size checks, worker-command timeout calculation, and worker-command reply mapping out of the daemon behind an explicit callback context, and reached a green checkpoint with `407 passed, no failures`.
- [x] (2026-04-30 18:21Z) Milestone 7: ran final structural checks and validation. The required side-effect and event-publisher grep checks produced no matches, the control-command structural grep also produced no matches, `daemon.gleam` is now `3,490` lines versus the `3,991` baseline, and final validation passed with `410 passed, no failures`.
- [x] (2026-04-30 20:06Z) Post-completion smoke validation against the junk Linear project passed: `--linear-smoke` reported `candidate_count=2 terminal_count=3 refreshed_count=1`, `--linear-contract-check` reported `linear_contract_ok`, `--pi-probe` reported `pi_probe_ok`, and a daemon/control smoke with `max_concurrent_agents: 0` successfully exercised `ping`, `ps`, `pause`, `resume`, `reload`, and graceful SIGTERM shutdown through the local control API.

## Surprises & Discoveries

- Observation: The current test suite intentionally prints Erlang `ERROR REPORT` blocks from tests that verify crashed workers are handled.
  Evidence: The current `direnv exec . gleam test` run printed crash reports from `test/orchestrator_daemon_test.gleam` and `test/workflow_run_test.gleam` but still ended with `376 passed, no failures`.

- Observation: `src/scherzo/orchestrator/core.gleam` is already a pure runtime-transition module and should not become the new god object.
  Evidence: It defines `core.Effect`, `core.Transition`, `core.new_state`, `core.apply_worker_success`, `core.apply_worker_failure`, `core.schedule_retry`, `core.reconcile_issue`, and invalid-workflow report state helpers, but it does not start processes, schedule timers, read files, or publish EventHub events.

- Observation: Some lifecycle decomposition has already happened outside the daemon.
  Evidence: `src/scherzo/orchestrator/service.gleam` already delegates graceful daemon stop behavior to `src/scherzo/lifecycle.gleam` and `src/scherzo/signal.gleam`. This plan should not reopen that work.

- Observation: The workspace metadata view changed during implementation.
  Evidence: Early plan review saw no visible `.git` or `.jj` metadata, but final `jj status --no-pager` succeeds and reports the working copy on top of `refactor(orchestrator): extracted control command handling from the daemon`; after post-completion smoke documentation, only `docs/plans/daemon-decomposition.md` is modified in the working copy.

- Observation: The current daemon does not define a `YamlRunHandle` or `yaml_run_monitors` field.
  Evidence: `State` in `src/scherzo/orchestrator/daemon.gleam` contains `yaml_step_runs` and `stopped_yaml_runs`, while monitor-backed process ownership is represented by `workers`, `worker_monitors`, `step_command_monitors`, and `step_command_subject_monitors`.

- Observation: A side-effect worker can crash too quickly for a plain spawn-then-monitor sequence to be fully deterministic in tests.
  Evidence: `src/scherzo/orchestrator/effect_runner.gleam` now uses a tiny start handshake: the child process creates a start subject, sends it to the runner, waits, and only runs the side effect after the runner has installed the monitor and sent the start signal.

- Observation: Worker registry extraction can be completed without moving EventHub publishing or worker process stopping into the registry.
  Evidence: `src/scherzo/orchestrator/worker_registry.gleam` owns dictionaries, route registration, monitor resolution, and cleanup, while `src/scherzo/orchestrator/daemon.gleam` still performs lifecycle event publication and process kill/abort side effects around registry calls.

- Observation: Poll and retry scheduling are separable even though both ultimately send messages back to the daemon actor.
  Evidence: `src/scherzo/orchestrator/poll_scheduler.gleam` owns generation, in-flight, and timer state with daemon-supplied scheduling/cancel callbacks; `src/scherzo/orchestrator/retry_scheduler.gleam` owns retry timer handles and refresh-in-flight guards while core runtime state still owns retry policy generations.

- Observation: Operator command handling can move without making daemon state public.
  Evidence: `src/scherzo/orchestrator/control_command_handler.gleam` is generic over the state type and receives explicit callbacks for state mutation, worker routing, and command-specific daemon actions. The daemon passes its private `State` into the generic context, avoiding an import cycle and preserving the existing public daemon API.

- Observation: Final line count remains above the plan's rough 2,500-line review threshold even after all planned state-owner extractions.
  Evidence: `wc -l src/scherzo/orchestrator/daemon.gleam` now reports `3,490`, down from `3,991`. The remaining code is largely integration glue for the single public daemon actor: startup/control-plane wiring, poll phase sequencing, dispatch policy orchestration around `core`, worker spawning and YAML workflow step callbacks, side-effect completion interpretation, handoff result handling, shutdown, and public message handling. These areas still belong together until a follow-up plan extracts worker lifecycle/poll phase orchestration or changes the public actor boundary.

## Decision Log

- Decision: Start with a monitored `EffectRunner` actor before cosmetic module extraction.
  Rationale: The side-effect queue has a real concurrency failure mode. Fixing it first makes the refactor safer and gives a clear behavioral acceptance test.
  Date: 2026-04-30

- Decision: Keep one public top-level daemon actor for now.
  Rationale: Existing callers and tests use `daemon.start`, `daemon.shutdown`, `daemon.get_snapshot`, and `daemon.apply_operator_command`. Splitting the public actor API is unnecessary to solve the god-object problem and would increase migration risk.
  Date: 2026-04-30

- Decision: Use modules for workflow reload, event publishing, worker registry, retry scheduling, and command handling before considering more actors.
  Rationale: These concerns need ownership boundaries and tests, but they do not currently require independent mailboxes. Adding actors without a concrete concurrency need would make ordering and testing harder.
  Date: 2026-04-30

- Decision: Do not persist side-effect queue state in this refactor.
  Rationale: Durable recovery is a separate hardening project. This plan makes the live daemon robust to side-effect process crashes while preserving the current non-durable semantics.
  Date: 2026-04-30

- Decision: Make validation and checkpoint instructions resilient to the actual checkout.
  Rationale: This review workspace has no visible VCS metadata, and direnv requires an explicit `direnv allow .` before first use. The implementation plan should prefer the project's direnv commands after allowing the reviewed `.envrc`, but it must not confuse an unallowed `.envrc` or absent `.git` directory with a code failure.
  Date: 2026-04-30

- Decision: Treat YAML step-session bookkeeping as the registry extraction target instead of inventing a `YamlRunHandle` abstraction.
  Rationale: The current daemon tracks YAML step sessions with `yaml_step_runs` and `stopped_yaml_runs`; there is no separate YAML run process monitor to move. The plan should extract current state accurately rather than prescribing stale types.
  Date: 2026-04-30

- Decision: Specify effect-runner monitor selection and crash-result mapping explicitly.
  Rationale: A runner that monitors workers but does not select monitor messages will still miss crashes, and leaving crash-to-result conversion implicit would force the implementer to invent failure semantics for each effect variant.
  Date: 2026-04-30

- Decision: Use a child start handshake in `effect_runner.spawn_effect_worker` before running each side effect.
  Rationale: Gleam's process monitor documentation says a monitor may not deliver a down message if the process is already dead when it is monitored. The handshake keeps the child alive until the runner records its monitor, making the crash-drain test deterministic without changing side-effect behavior.
  Date: 2026-04-30

- Decision: Keep `effect_runner` logging independent of reload-time secret changes and rely on the daemon's crash log for current redaction context.
  Rationale: The runner is intentionally isolated from daemon state. Its crash log only includes id, effect kind, and a synthetic reason string, while the daemon also logs `side_effect_crashed` through `log_state` using the current `workflow_reloader.State.secrets`.
  Date: 2026-04-30

- Decision: Make `worker_registry.Registry` opaque while keeping `worker_registry.WorkerHandle` public.
  Rationale: The daemon still needs to read worker handle fields when publishing lifecycle events, stopping processes, and finishing workers, but it no longer needs direct access to the underlying worker, monitor, step-command, YAML-step, or session-sequence dictionaries. This preserves behavior while making bookkeeping ownership explicit.
  Date: 2026-04-30

- Decision: Keep scheduler modules callback-based and generic over timer handles instead of importing daemon messages.
  Rationale: `poll_scheduler` and `retry_scheduler` can own timer bookkeeping without depending on `daemon.Message` or `daemon.TimerHandle`. The daemon supplies `send_after`/`cancel_timer` callbacks, avoiding import cycles and preserving the public daemon test helpers.
  Date: 2026-04-30

- Decision: Extract the operator command decision tree through a generic callback context rather than exposing `daemon.State`.
  Rationale: `control_command_handler` needs to decide which operator command path to take, but command effects still touch daemon-owned concerns such as reload application, retry dispatch, worker registry lookups, and EventHub publication. A generic context moves command parsing/validation/routing decisions out of `daemon.gleam` while keeping private daemon state private.
  Date: 2026-04-30

- Decision: Do not invent an extra final extraction solely to chase the rough 2,500-line threshold.
  Rationale: The mandatory ownership boundaries are now in place and the remaining large sections are behavior-rich integration code, especially worker lifecycle and poll/dispatch phase orchestration. Extracting those safely would require another plan with new characterization tests rather than an opportunistic cleanup commit.
  Date: 2026-04-30

## Outcomes & Retrospective

All planned milestones are complete. The concrete side-effect queue stall is fixed: `test/orchestrator_effect_runner_test.gleam` proves a panicking cleanup side effect emits `Crashed` and then drains a queued cleanup, and `daemon_side_effect_crash_does_not_stall_future_polls_test` proves a crashing candidate fetch no longer prevents a later poll from fetching candidates. Workflow reload state now lives in `workflow_reloader.State`, EventHub payload classification now lives in `event_publisher`, worker/YAML step/step-command route bookkeeping now lives in `worker_registry.Registry`, poll/retry timer bookkeeping now lives in scheduler modules, and the operator command decision tree now lives in `control_command_handler`.

Final structural checks passed for the required ownership boundaries: the daemon no longer contains `side_effects_in_flight`, `side_effect_queue`, `fn run_side_effect`, `fn spawn_side_effect`, `fn update_payload`, `fn kind_for_update`, or `fn publish_worker_update`. An additional control-command grep found no remaining direct operator command variant branches or moved helper definitions in `daemon.gleam`. Final validation passed with `direnv exec . gleam format --check src test` and `direnv exec . gleam test`, ending with `410 passed, no failures`.

The final daemon is `3,490` lines, a substantial reduction from the `3,991` verified baseline but still above the rough `2,500`-line review threshold. The remaining size is intentional for this plan: `daemon.gleam` is still the public integration actor and retains startup/control-plane wiring, poll phase sequencing, dispatch orchestration, worker spawning and YAML step callbacks, side-effect completion interpretation, handoff result handling, shutdown, and public message handling. A follow-up refactor should target worker lifecycle or poll/dispatch phase orchestration with fresh characterization tests rather than folding that extra work into this completed decomposition.

Additional smoke validation with real Linear credentials from the junk `~/Code/scherzo` project passed after automated acceptance. Read-only Linear smoke returned candidate, terminal, and refresh counts; Linear contract check returned `linear_contract_ok`; pi probe returned `pi_probe_ok`; and a daemon run using a temporary copy of the junk config with `max_concurrent_agents: 0` successfully served local control commands and handled graceful SIGTERM shutdown. No dispatch was allowed during that daemon smoke. Final `jj status --no-pager` succeeds and shows only this plan file modified after the implementation commits already present in the workspace.

## Context and Orientation

Scherzo is a Gleam service that runs pi agents from Linear issues. The repository targets Erlang, as shown by `target = "erlang"` in `gleam.toml`. The main daemon implementation is `src/scherzo/orchestrator/daemon.gleam`. A daemon is a long-running process that polls Linear, claims issues, starts agent workers, reports results, serves a local control API, and publishes session events.

The daemon actor's public entry points are `daemon.start(workflow_path, dependencies)`, `daemon.shutdown(subject, timeout_ms)`, `daemon.get_snapshot(subject, timeout_ms)`, and `daemon.apply_operator_command(subject, operator_command, timeout_ms)`. The actor receives a single large `Message` type that currently includes poll ticks, retry ticks, worker completions, worker updates, worker command readiness, YAML step events, monitor down events, side-effect completions, shutdown, snapshot requests, and operator commands.

`src/scherzo/orchestrator/core.gleam` owns pure runtime transitions. It defines `core.Effect` values such as `Dispatch`, `ScheduleRetry`, `CancelRetry`, `CleanupWorkspace`, `ReleaseClaim`, `StopWorker`, and `ParkIssue`. The daemon currently interprets those effects by scheduling timers, starting workers, and enqueueing side effects.

A side effect in this plan means work that may block or fail outside pure state transitions: tracker fetches, Linear command fetches and acknowledgements, handoff claim/success/failure reports, invalid-workflow triage reports, and workspace cleanup. Today these are represented by the private `SideEffect` type in `daemon.gleam`, run by `run_side_effect`, and completed through the public `SideEffectResult` type. A side-effect runner actor is a separate process that owns the queue and starts monitored worker processes for those effects.

An EventHub is Scherzo's in-memory session event stream. The daemon currently publishes lifecycle events and pi updates directly through `scherzo/session/hub`. A worker registry is the runtime bookkeeping that maps issue ids, session ids, and process monitors to active worker processes, YAML step-session-to-run mappings, stopped YAML run reasons, and YAML step command subjects.

The local control API is implemented in `src/scherzo/control/server.gleam`, `src/scherzo/control/command.gleam`, `src/scherzo/control/file.gleam`, and related modules. The daemon still owns the actor-side handling of those operator commands today.

## Preconditions and Verified Facts

The current tree has these relevant files:

- `src/scherzo/orchestrator/daemon.gleam`, verified at 3,991 lines with `wc -l`.
- `src/scherzo/orchestrator/core.gleam`, verified at 833 lines with `wc -l`.
- `src/scherzo/orchestrator/service.gleam`, verified at 839 lines with `wc -l`.
- `src/scherzo/control/linear_transport.gleam`, which already isolates Linear comment parsing, processed-comment state, and Linear command transport actions.
- `src/scherzo/tracker.gleam`, where `tracker.Client` is a record of tracker fetch functions.
- `src/scherzo/error.gleam`, where tracker and workspace error variants live.
- Daemon tests under `test/orchestrator_daemon_test.gleam`, `test/orchestrator_daemon_control_test.gleam`, `test/orchestrator_daemon_session_event_test.gleam`, and `test/orchestrator_daemon_linear_command_test.gleam`.

The preferred baseline command from the repository root is:

    direnv exec . gleam test

If the checkout has not allowed `.envrc` yet, first review `.envrc` and run:

    direnv allow .

When direnv is unavailable in a disposable workspace and Gleam is already on `PATH`, use this fallback from the repository root:

    gleam test

On 2026-04-30, after `direnv allow .`, the preferred command ended with:

    376 passed, no failures

The baseline also showed expected Erlang crash reports from tests that intentionally crash worker processes. Do not treat those reports as failures unless the final line reports failures or the command exits nonzero.

The current daemon side-effect implementation is in `src/scherzo/orchestrator/daemon.gleam` around the functions `enqueue_side_effect`, `drain_side_effects`, `max_side_effects`, `spawn_side_effect`, and `run_side_effect`. It uses `process.spawn_unlinked` for side-effect workers and increments `side_effects_in_flight` in the daemon state.

The current daemon worker implementation also uses `process.spawn_unlinked` in `spawn_worker`. This plan does not remove worker process spawning from the daemon in the first milestone; it only removes unmonitored side-effect queue ownership. Worker processes are already monitored through `worker_monitors`. YAML step command subjects are monitored through `step_command_monitors` and `step_command_subject_monitors`. YAML step sessions are tracked by `yaml_step_runs`, and stopped YAML run reasons are tracked by `stopped_yaml_runs`; there is no current `YamlRunHandle` or `yaml_run_monitors` field.

## Scope Boundaries

In scope: extracting the side-effect queue into a monitored runner actor; adding tests for side-effect crash recovery; moving workflow reload code into a workflow reloader module; moving EventHub event payload helpers into an event publisher module; moving worker, YAML step-run, and YAML step-command dictionaries into a worker registry module; moving poll and retry timer bookkeeping into scheduler modules; moving operator command decision logic into a command handler module; simplifying `State` in `daemon.gleam`; preserving all existing external daemon behavior.

Out of scope: durable crash recovery after BEAM death, host restart, `kill -9`, or power loss; persisting side-effect queues; changing Linear command syntax; changing EventHub event shape; changing local control API routes or authentication; changing workflow YAML semantics; changing pi worker internals; converting Scherzo to a full OTP application or supervision tree; adding distributed daemon coordination.

The final top-level daemon actor may still start workers, decide when to dispatch, and bridge between subsystems. It should not own every subsystem's private dictionaries and helper functions directly.

## Milestones

Milestone 0 adds characterization tests and records the baseline. During the red phase, the new side-effect crash tests should fail to compile or fail against the old implementation before the effect runner exists; do not commit or finish the milestone with a skipped or weakened safety test. If the daemon-level crash test cannot be made deterministic against the old implementation, record why in Surprises & Discoveries and keep the effect-runner unit test as the deterministic proof.

Milestone 1 introduces `src/scherzo/orchestrator/effect_runner.gleam`. At the end, side-effect queue state and side-effect process monitoring live outside `daemon.gleam`. A crashing side-effect worker frees runner capacity, emits a crash completion, logs `side_effect_crashed`, and does not prevent later queued effects or later polls from running.

Milestone 2 introduces `src/scherzo/orchestrator/workflow_reloader.gleam`. At the end, file reads, bundle reload, reload validation, last-known-good reload state, workflow definition selection, and secret recomputation are owned by that module. The daemon only asks the reloader to reload and then rebuilds runtime clients when the effective config changes.

Milestone 3 introduces `src/scherzo/orchestrator/event_publisher.gleam`. At the end, `publish_worker_update`, `publish_lifecycle`, `update_payload`, event kind classification, status classification, blocking UI method classification, and token nonzero checks are no longer in `daemon.gleam`.

Milestone 4 introduces `src/scherzo/orchestrator/worker_registry.gleam`. At the end, active worker handles, session lookup, monitor lookup, YAML step-session mappings, stopped YAML run reasons, step command subject lookup, command-ready registration, route clearing, and registry cleanup are owned by the registry. The daemon asks the registry to register, find, remove, and resolve monitor-down events.

Milestone 5 introduces scheduler modules for poll and retry timer state. At the end, `poll_generation`, `poll_in_flight`, `poll_timer`, `retry_timers`, and `retry_refreshes_in_flight` are no longer top-level daemon state fields. The daemon still receives `PollTick` and `RetryTick`, but acceptance, staleness checks, rescheduling, canceling, and generation bookkeeping are delegated.

Milestone 6 introduces `src/scherzo/orchestrator/control_command_handler.gleam`. At the end, operator command cases such as pause, resume, reload, retry, park, unpark, abort, stop-after-current-turn, prompt, and UI response live in that module with explicit dependencies and callbacks. The daemon still receives `ApplyOperatorCommand` and sends the reply, but it does not contain the large command decision tree.

Milestone 7 removes dead daemon code and writes the retrospective. At the end, the daemon remains the public integration actor, all tests pass, structural grep checks pass, and this plan records the remaining gaps.

## Plan of Work

Begin with tests because this refactor changes concurrency ownership. Add the side-effect crash tests before moving the queue. Then introduce `effect_runner.gleam` with the same side-effect variants and result variants currently in `daemon.gleam`, but with queue state, in-flight tracking, and monitor handling internal to the runner actor. Wire the daemon to start the runner during actor initialization and enqueue effects through `effect_runner.enqueue` instead of mutating `side_effect_queue`.

After the side-effect runner is stable, move code that has minimal coupling. Workflow reload is a good next extraction because it mostly depends on `runtime_bundle`, `config`, `simplifile`, and `domain`. Event publishing is also low risk because it can accept an event hub subject, session id, and update payload without seeing daemon state.

Then move registry state. This is more coupled because worker finish, worker down, YAML step-session lifecycle, command routing, shutdown, and EventHub publishing interact. Keep the migration incremental: first move the worker handle type and lookup functions, then YAML step-session bookkeeping, then command-ready route management, then monitor-down resolution, then shutdown cleanup. Run the daemon tests after each slice.

Next move scheduler state. Start with poll generation and staleness because that is small, then retry timers and retry refresh in-flight tracking. Do not move dispatch policy from `core.gleam`; keep pure runtime decisions where they already are.

Extract control command handling last because it depends on all earlier boundaries. Its public function should take a context record or explicit callbacks instead of importing daemon state. The daemon should call it, receive a new context plus `command.CommandResult`, log the result, send the reply, and continue.

## Concrete Steps

1. From the repository root, check for unrelated changes using whatever VCS metadata exists. If `.git` is present, run `git status --short`. If `.jj` is present, run `jj status`. If neither is present, record that this checkout has no visible VCS metadata and use the harness' file-diff view, if available, as the change boundary. If there are unrelated changes, stop and either commit/stash them or record exactly why they are safe to keep.

2. From the repository root, run `direnv exec . gleam test`. If it fails before running tests with `.envrc is blocked`, review `.envrc`, run `direnv allow .`, and retry `direnv exec . gleam test`. Use the fallback `gleam test` only when direnv is unavailable and Gleam is already on `PATH`. Expect the suite to end with `376 passed, no failures` in the current baseline. If the count differs because tests were added or removed, accept any count only if the command exits zero and reports `no failures`.

3. Create `test/orchestrator_effect_runner_test.gleam`. Add a test named `effect_runner_runs_successful_effect_once_test`. It should start the new runner with `max_concurrent: 1`, enqueue a `CleanupWorkspace` effect whose cleanup function sends `"cleanup_started"` to a test subject and returns `Ok(Nil)`, and assert that the notification subject receives exactly one `Finished` completion and no second completion within a short timeout.

4. In the same test file, add `effect_runner_reports_crash_and_drains_queue_test`. It should enqueue a first `CleanupWorkspace` effect whose cleanup function panics with `panic as "boom"`, then enqueue a second `CleanupWorkspace` effect whose cleanup function returns `Ok(Nil)`. Assert that the notification subject receives one `Crashed` completion for the first effect and then one `Finished` completion for the second effect. This test proves a crash frees queue capacity.

5. In `test/orchestrator_daemon_test.gleam`, add an integration test named `daemon_side_effect_crash_does_not_stall_future_polls_test`. Use a fake `tracker.Client` backed by a test subject so the first `fetch_candidate_issues` call sends `"fetch:1"` and panics, while the second call sends `"fetch:2"` and returns `Ok([])`. Start the daemon with `send_after` returning `daemon.TestTimer(delay)`, send `daemon.PollTick(1)`, wait for a `side_effect_crashed` log event, then send `daemon.PollTick(2)` and assert that `"fetch:2"` is observed. If the old daemon cannot make this test deterministic, mark the discovery in this plan and keep the effect-runner unit test as the deterministic crash proof.

6. Run the baseline test command from step 2. Before implementation, the new effect-runner tests will not compile because `src/scherzo/orchestrator/effect_runner.gleam` does not exist. This is the expected red phase.

7. Create `src/scherzo/orchestrator/effect_runner.gleam`. Define `pub type Effect` by moving the existing daemon-private side-effect variants: `FetchCandidates`, `FetchLinearCommands`, `RefreshRunning`, `RefreshRetry`, `ClaimIssue`, `ReportSuccess`, `ReportFailure`, `PostLinearCommandAck`, `ReportInvalidWorkflow`, and `CleanupWorkspace`. Define `pub type EffectResult` by moving the existing daemon `SideEffectResult` variants. Define `pub type Completion { Finished(id: Int, result: EffectResult) Crashed(id: Int, effect: Effect, reason: String) }`.

8. In `effect_runner.gleam`, define an actor-owned message type with `Enqueue(Effect)`, `WorkerFinished(Int, EffectResult)`, `WorkerDown(process.Down)`, and `Shutdown(process.Subject(Nil))`. Define a `Handle` type wrapping `process.Subject(Message)` and public functions `start`, `enqueue`, and `shutdown`. In `start`, build the actor selector with `process.new_selector() |> process.select(subject) |> process.select_monitors(WorkerDown)` so monitored side-effect process exits are actually delivered to the runner.

9. In `effect_runner.gleam`, implement internal state with `next_id: Int`, `queue: List(QueuedEffect)`, `in_flight: Dict(Int, InFlightEffect)`, `monitors: Dict(process.Monitor, Int)`, `max_concurrent: Int`, `notify: fn(Completion) -> Nil`, and `logger: fn(String, String, List(log.Field)) -> Nil`. `InFlightEffect` must store at least the side-effect id, the original `Effect`, the worker `process.Pid`, and the worker `process.Monitor` so shutdown and monitor-down handling can clean up precisely. Keep `max_concurrent` configurable but use `4` from production daemon wiring to preserve current behavior.

10. Move `run_side_effect` logic from `daemon.gleam` into `effect_runner.gleam`. Keep the behavior of every successful side effect unchanged: tracker fetches call the same client functions, handoff calls the same client functions, invalid workflow reports call the same triage client, and cleanup calls the supplied cleanup function.

11. Implement runner draining so it starts queued effects while `dict.size(in_flight) < max_concurrent`. Each worker must be started with `process.spawn_unlinked`, monitored immediately, and tracked by id, pid, effect, and monitor. On normal `WorkerFinished`, demonitor the process, remove the in-flight entry, call `notify(Finished(id, result))`, and drain again. On `WorkerDown` for a known monitor, remove the in-flight entry, convert `process.Normal` to `"side_effect_exited_without_result"`, `process.Killed` to `"side_effect_killed"`, and `process.Abnormal(_)` to `"side_effect_crashed"`; then call `notify(Crashed(id, effect, reason))`, log `side_effect_crashed` with `reason`, and drain again. Ignore stale monitor downs and stale `WorkerFinished` messages for ids no longer in flight.

12. Update `src/scherzo/orchestrator/daemon.gleam` to import `scherzo/orchestrator/effect_runner`. Replace the daemon `SideEffect` and `SideEffectResult` types with uses of `effect_runner.Effect`, `effect_runner.EffectResult`, and `effect_runner.Completion`. Change the daemon message from `SideEffectFinished(SideEffectResult)` to `SideEffectCompleted(effect_runner.Completion)`.

13. In `daemon.start`, start an effect runner inside the actor initializer after the daemon subject is available. Pass `max_concurrent: 4`, a logger that calls the daemon logger with current secrets, and a notify function that sends `SideEffectCompleted(completion)` to the daemon subject. Store the returned `effect_runner.Handle` in daemon state.

14. Replace `enqueue_side_effect`, `drain_side_effects`, `max_side_effects`, `spawn_side_effect`, and `run_side_effect` in `daemon.gleam` with a small `enqueue_side_effect` wrapper that calls `effect_runner.enqueue(state.effect_runner, effect)` and returns `state`. Remove `side_effects_in_flight` and `side_effect_queue` from daemon `State`.

15. Replace `handle_side_effect_finished` with `handle_side_effect_completed`. For `Finished(_, result)`, call the existing result-specific handlers. For `Crashed(_, effect, reason)`, log `side_effect_crashed` with an effect kind and `reason`, then synthesize the exact error result the old handlers already understand: `FetchCandidates(generation, _)` becomes `CandidateFetchFinished(generation, Error(error.LinearApiRequest(reason)))`; `FetchLinearCommands(generation, _, candidates, dispatch_after, _, _)` becomes `LinearCommandFetchFinished(generation, candidates, dispatch_after, Error(error.LinearApiRequest(reason)))`; `RefreshRunning(generation, _, _)` becomes `RunningRefreshFinished(generation, Error(error.LinearApiRequest(reason)))`; `RefreshRetry(issue_id, generation, _)` becomes `RetryRefreshFinished(issue_id, generation, Error(error.LinearApiRequest(reason)))`; `ClaimIssue(issue, _, run_id, _)` becomes `HandoffClaimFinished(issue.id, run_id, Error(error.LinearApiRequest(reason)))`; `ReportSuccess(issue_id, _, _, run_id, _)` becomes `HandoffSuccessFinished(issue_id, run_id, Error(error.LinearApiRequest(reason)))`; `ReportFailure(issue_id, _, _, run_id, _)` becomes `HandoffFailureFinished(issue_id, run_id, Error(error.LinearApiRequest(reason)))`; `PostLinearCommandAck(issue_id, source_comment_id, _, _)` becomes `LinearCommandAckFinished(issue_id, source_comment_id, Error(error.LinearApiRequest(reason)))`; `ReportInvalidWorkflow(issue, _, violation_fingerprint, reporting_policy_fingerprint, _)` becomes `InvalidWorkflowReportFinished(issue.id, violation_fingerprint, reporting_policy_fingerprint, Error(error.LinearApiRequest(reason)))`; and `CleanupWorkspace(_, workspace_path, _, _)` becomes `CleanupFinished(workspace_path, Error(error.WorkspaceIo(reason)))`. Then call the same result-specific handler so poll, retry, pending claim, invalid workflow, ack, and cleanup paths complete normally.

16. In `shutdown_state`, call `effect_runner.shutdown(state.effect_runner, 1000)` before clearing daemon runtime fields. The runner shutdown handler should iterate over stored `InFlightEffect` values, call `process.demonitor_process(monitor)`, call `process.kill(pid)`, drop queued effects, send the ack, and then stop the runner actor. If a stale completion reaches the daemon after shutdown has begun, the daemon should ignore it rather than re-enqueueing work.

17. Run the validation command sequence from Validation and Acceptance. Expect all tests to pass. The pass count should be at least the current baseline plus the new tests. Commit this milestone with a message such as `Extract monitored side-effect runner` when VCS metadata is available; otherwise record this as a green checkpoint in Progress.

18. Create `src/scherzo/orchestrator/workflow_reloader.gleam`. Move `workflow_definition_from_bundle`, `reload_if_changed`, `apply_new_contents`, `validate_reloaded_bundle`, `apply_reloaded_bundle`'s pure reload-state construction, and `mark_reload_invalid` logic into this module. Define a `State` containing `workflow_path`, `chosen_path`, `last_contents`, `bundle`, `definition`, `reload_state`, `effective`, and `secrets`.

19. In `workflow_reloader.gleam`, expose `from_bundle(workflow_path, bundle)`, `reload_if_changed(state)`, and `reload_now(state)`. Return an `Outcome` that distinguishes unchanged, reloaded, and invalid states. The module should not create tracker, handoff, Linear command, or triage clients; it only returns the new effective config and secrets so the daemon can rebuild clients.

20. Add `test/orchestrator_workflow_reloader_test.gleam`. Write one test that loads a workflow and confirms unchanged contents return an unchanged outcome. Write a second test that changes polling interval in the workflow file and confirms `reload_if_changed` returns a reloaded state with the new interval. Write a third test that writes invalid config and confirms the returned reload state is `config.CurrentInvalid` while the previous effective config remains the last known good config.

21. Update `daemon.gleam` so workflow-related fields are replaced by `workflow: workflow_reloader.State` where practical. If a single commit replacing all fields is too large, first call `workflow_reloader` functions while keeping the old fields, then collapse the fields in a second commit. Rebuild tracker, handoff, Linear command, and triage clients in the daemon only when the reloader returns a valid reloaded effective config.

22. Run format and tests. Commit this milestone with a message such as `Extract daemon workflow reloader`.

23. Create `src/scherzo/orchestrator/event_publisher.gleam`. Move `publish_worker_update`, `publish_lifecycle`, `update_payload`, `kind_for_update`, `pi_type_for_update`, `status_for_update`, `is_blocking_ui_method`, and `tokens_are_nonzero` into this module. Expose `worker_update(event_hub, session_id, update)` and `lifecycle(event_hub, session_id, name, message)`.

24. Add `test/orchestrator_event_publisher_test.gleam` or extend `test/session_event_test.gleam` with direct tests for event kind classification. Include at least one raw pi event, one blocking `extension_ui_request` with method `input`, one non-blocking UI method, one tool-shaped `message`, and one `turn_finished` token stats event.

25. Update `daemon.gleam` to call `event_publisher.worker_update` and `event_publisher.lifecycle`. Delete the moved helper functions from `daemon.gleam`.

26. Run format and tests. Commit this milestone with a message such as `Extract daemon event publisher`.

27. Create `src/scherzo/orchestrator/worker_registry.gleam`. Move `WorkerHandle` into this module unless a public compatibility issue is found. Define `Registry` with workers, worker monitors, issue sessions, YAML step-session mappings, stopped YAML run reasons, step command subjects, step command monitors, step command subject monitors, and next session sequence. Do not introduce a `YamlRunHandle` or `yaml_run_monitors` abstraction unless new code first creates a real YAML run process handle that needs one.

28. In `worker_registry.gleam`, implement `new`, `reserve_session_sequence`, `register_worker`, `register_worker_command_subject`, `register_yaml_step_started`, `finish_yaml_step`, `active_yaml_step_sessions_for_run`, `mark_yaml_run_stopping`, `stopped_yaml_run_reason`, `clear_yaml_step_command_route`, `worker_for_session`, `active_issue_ids`, `active_issues`, `has_active_run`, `remove_worker`, `remove_all`, and `resolve_down`. `resolve_down` should return a value that tells the daemon whether a down monitor belonged to a worker, YAML step command subject, or nothing known.

29. Add `test/orchestrator_worker_registry_test.gleam`. Cover registering a worker and looking it up by session id, registering a YAML step session and finding all active step sessions for a run id, marking a YAML run as stopping and reading its reason, registering and clearing a YAML step command subject, resolving worker and step-command monitor downs, and `remove_all` demonitoring or forgetting all entries without leaving session mappings behind.

30. Update `daemon.gleam` in small slices. First replace lookup helpers such as `worker_for_session`, `worker_for_run`, `active_run_issue_ids`, `active_run_issues`, `has_active_run`, and first-worker helpers with registry calls. Run tests. Then replace YAML step-session helpers such as `handle_yaml_step_started`, `handle_yaml_step_finished`, `active_yaml_step_sessions_for_run`, and `finish_yaml_step_sessions_for_run`. Run tests. Then replace command-ready route management. Run tests. Then replace monitor-down handling. Run tests. Then replace shutdown registry cleanup. Run tests.

31. Commit each green slice or one green milestone, depending on diff size. Suggested final milestone message: `Extract daemon worker registry`.

32. Create `src/scherzo/orchestrator/poll_scheduler.gleam`. Define a generic `State(timer)` with poll generation, in-flight generation, and current poll timer. Expose functions `new(initial_timer, initial_generation)`, `accept_tick`, `mark_finished_and_schedule`, `cancel`, and `is_stale`. Keep this module generic over `timer` so it does not import `daemon.TimerHandle`.

33. Create `src/scherzo/orchestrator/retry_scheduler.gleam`. Define a generic `State(timer)` with retry timers and retry refreshes in flight. Expose functions to schedule a retry timer, cancel a timer, mark a retry refresh started, determine whether a retry refresh completion is stale, finish a refresh, and cancel all timers.

34. Add `test/orchestrator_poll_scheduler_test.gleam` and `test/orchestrator_retry_scheduler_test.gleam`. Use a simple fake timer type such as `FakeTimer(String)`. Assert stale poll ticks are rejected, accepted poll ticks set in-flight state, finishing a poll increments generation, canceling a retry removes only that issue id, and stale retry refresh generations are ignored.

35. Update `daemon.gleam` so `poll_generation`, `poll_in_flight`, `poll_timer`, `retry_timers`, and `retry_refreshes_in_flight` move into scheduler state fields. Keep the `PollTick` and `RetryTick` message constructors unchanged for compatibility. Replace manual timer insert/delete code in `handle_poll_tick`, `schedule_next_poll`, `handle_retry_tick`, `defer_retry_until_dispatch_available`, `begin_retry_refresh`, `handle_retry_refresh_finished`, `cancel_retry_timer`, and `shutdown_state` with scheduler calls.

36. Run format and tests. Commit this milestone with a message such as `Extract daemon poll and retry schedulers`.

37. Create `src/scherzo/orchestrator/control_command_handler.gleam`. Move pure command helpers first: `worker_command_timeout`, `operator_prompt_too_large`, `ui_response_too_large`, and `worker_reply_to_command_result`. Add direct tests for boundary sizes and reply mapping in `test/orchestrator_control_command_handler_test.gleam`.

38. Move issue resolution helpers into `control_command_handler.gleam`: local issue lookup, unique issue resolution, parked issue id lookup, tracker fetch by id, and tracker fetch by identifier. Keep tracker fetches explicit in a dependency record so tests can provide fake clients. Add tests for not found, ambiguous identifier, fetch failure, and local match taking precedence over remote fetch.

39. Move command cases one group at a time. First move `PauseDispatch`, `ResumeDispatch`, and `ReloadWorkflow`; then `RetryIssue`, `ParkIssue`, and `UnparkIssue`; then command routing for `AbortSession`, `StopAfterCurrentTurn`, `PromptSession`, and `RespondUi`. Use a handler context that contains runtime state, workflow reload state, worker registry, pending claims, operator paused flag, tracker client, and callbacks for dispatching, stopping, and sending worker commands. Do not let this module import `daemon.gleam`.

40. Update `daemon.gleam` so `handle_operator_command` calls `control_command_handler.apply`, logs the returned command result, sends the reply, and stores the returned context-derived state. Preserve the existing command result statuses and messages verified by `test/orchestrator_daemon_control_test.gleam` and `test/orchestrator_daemon_linear_command_test.gleam`.

41. Run format and tests. Commit this milestone with a message such as `Extract daemon control command handler`.

42. Perform final cleanup in `daemon.gleam`. Remove dead helper functions, remove unused imports, collapse nested state fields where the new modules now own state, and make helper names reflect orchestration glue. Do not change behavior in this cleanup commit.

43. Run structural checks from the repository root:

    grep -E -n "side_effects_in_flight|side_effect_queue|fn run_side_effect|fn spawn_side_effect" src/scherzo/orchestrator/daemon.gleam
    grep -E -n "fn update_payload|fn kind_for_update|fn publish_worker_update" src/scherzo/orchestrator/daemon.gleam
    wc -l src/scherzo/orchestrator/daemon.gleam

    The first two grep commands should print no matches; with standard `grep`, exit status `1` with no output is the expected success case for these absence checks. The line count should be substantially below 3,991; if it is still above roughly 2,500 lines, stop and record why the remaining code belongs in the daemon before calling this complete.

44. Run final validation through direnv:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

    If direnv reports `.envrc is blocked`, review `.envrc`, run `direnv allow .`, and retry the direnv commands. If direnv is unavailable in a disposable workspace and Gleam is already on `PATH`, use the fallback commands:

    gleam format --check src test
    gleam test

    Expect both commands to exit zero and the test command to report `no failures`. The exact passed count should be the current baseline plus the tests added during this plan unless other concurrent work changes the count.

45. Update this plan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective. Commit the cleanup and plan update with a message such as `Document daemon decomposition outcome` when VCS metadata is available; otherwise record the green checkpoint and the absence of VCS metadata in Progress.

## Testing and Falsifiability

The main falsifiable safety claim is: a side-effect process crash no longer stalls the side-effect queue or leaves the daemon poll phase stuck forever. The deterministic unit proof is `test/orchestrator_effect_runner_test.gleam` with `effect_runner_reports_crash_and_drains_queue_test`: the first effect panics, the second effect still runs, and the runner emits both a crash completion and a success completion. The daemon integration proof is `daemon_side_effect_crash_does_not_stall_future_polls_test`: after a crashing first candidate fetch, a later poll tick causes another fetch. If either test fails, the plan has not fixed the concrete concurrency bug.

The behavior-preservation claim is falsified by any regression in the existing suite. The most important parity tests are the daemon tests for dispatch, retries, YAML workflow sessions, Linear operator commands, session events, control server commands, lifecycle shutdown, and service lifecycle. Existing tests must keep their current assertions; do not weaken tests to make extraction easier.

The module-boundary claim is falsified if the final daemon still contains the side-effect queue implementation, event payload classification helpers, workflow reload implementation, retry timer dictionaries, worker registry dictionaries, and control command decision tree. The structural grep checks in Concrete Step 43 are mandatory. Line count alone is not acceptance, but a final daemon close to the current 3,991-line baseline means the extraction did not accomplish the maintainability goal.

For new tests, use these concrete scenarios:

- In `test/orchestrator_effect_runner_test.gleam`, assert a successful cleanup effect emits exactly one `Finished` completion and no duplicate completion after its process exits.
- In `test/orchestrator_effect_runner_test.gleam`, assert a panicking cleanup effect emits `Crashed`, then a queued cleanup effect emits `Finished`.
- In `test/orchestrator_workflow_reloader_test.gleam`, assert unchanged contents do not reload, valid changed contents reload and update `effective.polling.interval_ms`, and invalid contents mark `config.CurrentInvalid` without discarding the last known good config.
- In `test/orchestrator_event_publisher_test.gleam` or `test/session_event_test.gleam`, assert the moved event classifier still maps blocking UI requests to `session_event.UiRequest`, UI responses to `session_event.UiResponse`, tool-shaped messages to `session_event.Tool`, `turn_finished` to `session_event.TokenStats`, and raw unknown events to `session_event.PiRaw`.
- In `test/orchestrator_worker_registry_test.gleam`, assert worker registration, YAML step-session registration, stopped YAML run reason tracking, monitor resolution, session lookup, and route clearing remove all relevant maps.
- In `test/orchestrator_poll_scheduler_test.gleam`, assert stale poll ticks are rejected and finishing a poll clears in-flight state and increments generation.
- In `test/orchestrator_retry_scheduler_test.gleam`, assert canceling one retry timer does not delete another issue's timer and stale retry refresh completions are ignored.
- In `test/orchestrator_control_command_handler_test.gleam`, assert prompt and UI response size guards reject oversized payloads, worker replies map to the same command statuses, issue identifier resolution rejects ambiguous matches, and remote tracker fetch errors produce the same rejection reasons as the current daemon.

## Validation and Acceptance

Acceptance requires the validation command sequence to pass from the repository root. Prefer these commands through direnv:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

If `.envrc` is blocked, review `.envrc`, run `direnv allow .`, and retry the direnv commands. Use these fallback commands only when direnv is unavailable in a disposable workspace and Gleam is already on `PATH`:

    gleam format --check src test
    gleam test

Both commands must exit zero. `gleam test` must report `no failures`. Expected Erlang crash reports from tests that intentionally crash worker or side-effect processes are acceptable only when the final test summary is green.

Behavior acceptance requires `daemon.start`, `daemon.shutdown`, `daemon.get_snapshot`, and `daemon.apply_operator_command` to retain their existing callers. The service lifecycle tests must still prove daemon mode handles graceful SIGTERM through `service.start_daemon_with_lifecycle`. The control tests must still prove pause, resume, reload, retry, park, unpark, abort, prompt, and UI response commands work through the daemon actor and Linear command transport.

Structural acceptance requires these checks:

    grep -E -n "side_effects_in_flight|side_effect_queue|fn run_side_effect|fn spawn_side_effect" src/scherzo/orchestrator/daemon.gleam
    grep -E -n "fn update_payload|fn kind_for_update|fn publish_worker_update" src/scherzo/orchestrator/daemon.gleam

Both `grep` commands must print no matches; exit status `1` with no output is success for these absence checks. `wc -l src/scherzo/orchestrator/daemon.gleam` must show a substantial reduction from 3,991 lines. If the final daemon remains above roughly 2,500 lines, the implementer must record in Outcomes & Retrospective which concerns still remain and why they were intentionally left there.

Safety acceptance requires the new side-effect crash tests to pass. The runner must not stall when a side-effect worker crashes. The daemon must log a crash and continue poll/retry flow instead of leaving an in-flight side effect counted forever.

## Rollout, Recovery, and Idempotence

This is an internal refactor with one bug fix. There is no data migration and no operator rollout switch. Each milestone should be committed only after tests pass when VCS metadata is available, so the change can be backed out one milestone at a time. In a checkout with no visible VCS metadata, treat those commit points as explicit green checkpoints in Progress. If a late extraction causes confusing failures, revert the last milestone rather than debugging across several uncommitted moves.

The side-effect runner changes live concurrency behavior. If they cause production trouble, the rollback is to revert the `EffectRunner` milestone and return to the old daemon-local queue while investigating. That rollback reintroduces the known queue-stall risk, so keep the crash tests in the tree if possible and mark them pending only as a temporary measure if rollback is necessary.

Module extraction steps are idempotent in the sense that running format and tests repeatedly is safe. Test temporary files should remain under `test/tmp/...`, matching existing repository convention. Do not create new persistent files outside `src/`, `test/`, and this plan unless a milestone explicitly records why.

## Artifacts and Notes

Baseline command recorded during plan authoring:

    direnv exec . gleam test
    ...
    377 passed, no failures

Current review validation note:

    direnv allow .
    direnv exec . gleam test
    ...
    376 passed, no failures

Current size check recorded during plan review:

    wc -l src/scherzo/orchestrator/daemon.gleam src/scherzo/orchestrator/core.gleam src/scherzo/orchestrator/service.gleam
      3991 src/scherzo/orchestrator/daemon.gleam
       833 src/scherzo/orchestrator/core.gleam
       839 src/scherzo/orchestrator/service.gleam
      5663 total

Current side-effect queue code to remove from the daemon is around `src/scherzo/orchestrator/daemon.gleam` functions `enqueue_side_effect`, `drain_side_effects`, `max_side_effects`, `spawn_side_effect`, and `run_side_effect`.

Current event publishing code to move from the daemon is around `publish_worker_update`, `publish_lifecycle`, `update_payload`, `kind_for_update`, `pi_type_for_update`, `status_for_update`, `is_blocking_ui_method`, and `tokens_are_nonzero`.

## Interfaces and Dependencies

In `src/scherzo/orchestrator/effect_runner.gleam`, define these public shapes:

    pub type Effect {
      FetchCandidates(generation: Int, client: tracker.Client)
      FetchLinearCommands(
        generation: Int,
        issue_ids: List(String),
        candidates: List(domain.Issue),
        dispatch_after: Bool,
        client: linear.CommandClient,
        limit_per_issue: Int,
      )
      RefreshRunning(generation: Int, ids: List(String), client: tracker.Client)
      RefreshRetry(issue_id: String, generation: Int, client: tracker.Client)
      ClaimIssue(issue: domain.Issue, workspace_path: String, run_id: String, client: handoff.Client)
      ReportSuccess(issue_id: String, issue: domain.Issue, success: runner.WorkerSuccess, run_id: String, client: handoff.Client)
      ReportFailure(issue_id: String, issue: domain.Issue, failure: runner.WorkerFailure, run_id: String, client: handoff.Client)
      PostLinearCommandAck(issue_id: String, source_comment_id: String, body: String, client: linear.CommandClient)
      ReportInvalidWorkflow(issue: domain.Issue, violation: workflow_policy.IssueWorkflowViolation, violation_fingerprint: String, reporting_policy_fingerprint: String, client: linear_triage.TriageClient)
      CleanupWorkspace(root: String, workspace_path: String, hooks: domain.HooksConfig, cleanup: fn(String, String, domain.HooksConfig) -> Result(Nil, error.WorkspaceError))
    }

    pub type EffectResult {
      CandidateFetchFinished(Int, Result(List(domain.Issue), error.TrackerError))
      LinearCommandFetchFinished(Int, List(domain.Issue), Bool, Result(List(linear.LinearComment), error.TrackerError))
      RunningRefreshFinished(Int, Result(List(domain.Issue), error.TrackerError))
      RetryRefreshFinished(String, Int, Result(List(domain.Issue), error.TrackerError))
      HandoffClaimFinished(String, String, Result(Nil, error.TrackerError))
      HandoffSuccessFinished(String, String, Result(Nil, error.TrackerError))
      HandoffFailureFinished(String, String, Result(Nil, error.TrackerError))
      LinearCommandAckFinished(String, String, Result(Nil, error.TrackerError))
      InvalidWorkflowReportFinished(String, String, String, Result(linear_triage.InvalidWorkflowReportOutcome, error.TrackerError))
      CleanupFinished(String, Result(Nil, error.WorkspaceError))
    }

    pub type Completion {
      Finished(id: Int, result: EffectResult)
      Crashed(id: Int, effect: Effect, reason: String)
    }

    pub type Dependencies {
      Dependencies(
        max_concurrent: Int,
        notify: fn(Completion) -> Nil,
        logger: fn(String, String, List(log.Field)) -> Nil,
      )
    }

    pub opaque type Handle {
      Handle(subject: process.Subject(Message))
    }

    pub fn start(dependencies: Dependencies) -> Result(Handle, Nil)
    pub fn enqueue(handle: Handle, effect: Effect) -> Nil
    pub fn shutdown(handle: Handle, timeout_ms: Int) -> Result(Nil, Nil)

The effect runner actor must select both its subject and monitor messages with `process.select_monitors(WorkerDown)`. Its private in-flight record must store the original effect, pid, and monitor so `WorkerDown`, normal finish, and shutdown can all remove the same work item without duplicate completion notifications.

In `src/scherzo/orchestrator/workflow_reloader.gleam`, define a state type that owns workflow path, chosen path, last contents, bundle, definition, reload state, effective config, and secrets. Expose reload functions that return an outcome and do not create network clients.

In `src/scherzo/orchestrator/event_publisher.gleam`, expose only event publishing and event classification functions. It should import `scherzo/session/hub`, `scherzo/session/event`, `scherzo/agent/runner`, and `scherzo/domain`; it should not import `daemon.gleam`.

In `src/scherzo/orchestrator/worker_registry.gleam`, define `Registry`, `WorkerHandle`, monitor/session lookup functions, YAML step-session functions, and stopped YAML run reason functions. It may import `gleam/erlang/process`, `gleam/dict`, `scherzo/domain`, and `scherzo/agent/worker_command`; it should not import `daemon.gleam`. It should not define `YamlRunHandle` unless the implementation first introduces a real YAML run process handle that is absent from the current daemon.

In scheduler modules, keep timer state generic over the timer handle type or accept callbacks. Do not import `daemon.TimerHandle` from scheduler modules.

In `src/scherzo/orchestrator/control_command_handler.gleam`, define an explicit context and dependencies record. It may import `scherzo/control/command`, `scherzo/agent/worker_command`, `scherzo/domain`, `scherzo/tracker`, `scherzo/orchestrator/core`, `scherzo/orchestrator/worker_registry`, and `scherzo/orchestrator/workflow_reloader`. It must not import `scherzo/orchestrator/daemon`.
