# Decompose the daemon into focused orchestration subsystems

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo daemon mode should be easier and safer to change because the top-level daemon actor in `src/scherzo/orchestrator/daemon.gleam` will be orchestration glue instead of the implementation home for polling, retry timers, worker bookkeeping, workflow reloads, event publishing, Linear command routing, control commands, and side-effect execution. Operators should not see a feature change: the same daemon CLI, local control API, Linear command behavior, EventHub stream, worker lifecycle, retry policy, and workflow reload behavior must continue to work.

The visible proof is twofold. First, behavior stays green: from the repository root, `direnv exec . gleam test` continues to pass, daemon control tests still exercise the same public API, and targeted new tests prove side-effect worker crashes no longer stall the daemon. Second, the structure changes in an observable way: `src/scherzo/orchestrator/daemon.gleam` no longer owns the side-effect queue fields, side-effect spawning code, workflow reload implementation, event payload classification helpers, retry timer bookkeeping, or worker registry dictionaries directly. A reader adding a future daemon feature should be able to locate the relevant subsystem module without editing thousands of lines of unrelated actor code.

## Problem Framing and Constraints

The current daemon actor is a god object: one file, `src/scherzo/orchestrator/daemon.gleam`, is 4,184 lines and owns nearly every runtime concern. It defines the actor `Message` type, worker handles, YAML run handles, timer handles, control server handles, pending claims, side-effect variants, side-effect result variants, runtime dependencies, and one broad `State` record. The same file starts the control server, schedules poll ticks, reloads workflows, fetches tracker data, fetches Linear commands, dispatches workers, routes operator commands, publishes EventHub events, runs YAML workflow steps, schedules retries, performs handoff reporting, reports invalid workflow candidates, cleans workspaces, and shuts everything down.

This hurts operators indirectly. When unrelated behavior shares one actor and one state record, small changes increase the chance of stale state, missed cleanup, blocked command handling, and lifecycle bugs. The most concrete current safety risk is the side-effect queue: `src/scherzo/orchestrator/daemon.gleam` tracks `side_effects_in_flight` and `side_effect_queue`, then starts work with `process.spawn_unlinked`. If one side-effect process crashes before it sends `SideEffectFinished`, the daemon never decrements its in-flight count for that process. After enough crashes, the queue can stop draining even though the daemon actor itself is still alive.

This plan must preserve Scherzo's current Erlang-target Gleam architecture. It must not turn this refactor into a full OTP application rewrite, a durable crash-recovery project, a new workflow engine, or a change to user-visible daemon semantics. The daemon may remain the single top-level actor that receives public control messages; the important change is that it delegates coherent subsystems to focused modules and one focused side-effect runner actor.

## Strategy Overview

Use a strangler refactor. Add focused modules around one concern at a time, route the existing daemon through the new module, run the full suite, then delete the old daemon-local implementation for that concern. Start with the side-effect runner because it has a correctness risk, not just a taste issue. Then extract low-risk modules whose interfaces are clear: workflow reload, event publishing, worker registry, poll/retry scheduling, and control command handling.

The top-level daemon actor should remain responsible for public compatibility: `daemon.start`, `daemon.shutdown`, `daemon.get_snapshot`, and `daemon.apply_operator_command` continue to exist. Existing tests that construct `daemon.RuntimeDependencies`, use `daemon.TestTimer`, or return `daemon.NoControlServer` should either keep working unchanged or be updated in the same commit that deliberately moves those public helper types. Prefer keeping these compatibility types in `daemon.gleam` until the final cleanup, because this plan is about decomposing internals, not forcing a broad test API migration.

The new structure should be proportionate. Only the side-effect subsystem needs its own actor because it owns concurrent work and monitors. The worker registry, workflow reloader, event publisher, and scheduler can start as ordinary modules with explicit state and functions. If those modules later need actors, the boundaries will already be clear.

## Alternatives Considered

The simplest option is to leave the daemon as one file and add comments or region markers. That does not solve the side-effect crash stall, does not reduce the state surface touched by each feature, and still leaves every daemon change competing inside one 4,184-line module.

Another option is a full rewrite into a supervision tree with many OTP actors at once. That may eventually be attractive, but it is too risky for this problem. The existing daemon has broad behavior and a large test suite; a big-bang actor rewrite would make regressions hard to isolate. This plan introduces only one new actor where concurrency ownership is needed now.

A third option is to extract only pure helper functions while leaving the broad state record and side-effect queue in the daemon. That would reduce line count slightly but leave the architectural weakness intact. This plan moves ownership of state subsets, not just syntax.

A fourth option is to merge more behavior into `src/scherzo/orchestrator/core.gleam`. That module already owns pure runtime transitions such as worker success/failure, retry entries, reconciliation, parking, and invalid-workflow reporting. It should remain pure domain logic. Runtime processes, timers, monitors, file reloads, control command routing, and EventHub publishing should live in separate orchestration modules instead of bloating `core.gleam`.

## Risks and Countermeasures

The largest regression risk is changing lifecycle ordering while moving code. Countermeasure: each milestone is behavior-preserving except the side-effect crash fix, and each milestone ends with `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Existing daemon tests in `test/orchestrator_daemon_test.gleam`, `test/orchestrator_daemon_control_test.gleam`, `test/orchestrator_daemon_session_event_test.gleam`, `test/orchestrator_daemon_linear_command_test.gleam`, and `test/orchestrator_service_lifecycle_test.gleam` remain the primary parity suite.

The side-effect runner can accidentally deliver duplicate completions if a worker sends its result and then the monitor down message is also processed. Countermeasure: give every side effect a numeric id, keep an in-flight map by monitor and id, demonitor on normal finish, ignore stale monitor downs, and add a unit test that a successful effect produces exactly one completion.

The side-effect runner can hide crashes by only freeing queue capacity. Countermeasure: it must notify the daemon with `effect_runner.Crashed(id, effect, reason)`. The daemon must log `side_effect_crashed` and synthesize the same kind of failure result that the old handler already knows how to process, so poll and retry phases finish instead of staying in flight.

Extracted modules can create import cycles. Countermeasure: lower-level modules must not import `scherzo/orchestrator/daemon`. If a module needs to notify the daemon, it accepts a callback function such as `fn(effect_runner.Completion) -> Nil`. If a scheduler needs timers, it is generic over the timer handle type or accepts timer callbacks supplied by the daemon.

The public test helper API can churn. Countermeasure: keep `RuntimeDependencies`, `TimerHandle`, and `ControlServerHandle` in `daemon.gleam` during the main extraction. Move them only if the final cleanup proves it is worth the compatibility cost, and update all references in the same commit.

The final daemon can remain too large if extractions stop halfway. Countermeasure: final acceptance includes structural checks: `grep -n "side_effects_in_flight\|side_effect_queue\|fn run_side_effect\|fn spawn_side_effect" src/scherzo/orchestrator/daemon.gleam` should return no matches, `grep -n "fn update_payload\|fn kind_for_update\|fn publish_worker_update" src/scherzo/orchestrator/daemon.gleam` should return no matches, and `wc -l src/scherzo/orchestrator/daemon.gleam` should show a substantial reduction from the verified baseline of 4,184 lines. The exact final count is less important than the ownership boundaries, but a daemon still above roughly 2,500 lines should trigger a review before calling this complete.

## Progress

- [x] (2026-04-30 10:47Z) Verified the current repository baseline with `direnv exec . gleam test`; it passed with `377 passed, no failures`.
- [x] (2026-04-30 11:00Z) Drafted this plan against the current tree after reading `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/core.gleam`, `src/scherzo/orchestrator/service.gleam`, `src/scherzo/control/linear_transport.gleam`, `src/scherzo/tracker.gleam`, and `src/scherzo/error.gleam`.
- [ ] Milestone 0: add characterization tests for side-effect crash recovery and current daemon parity.
- [ ] Milestone 1: introduce `src/scherzo/orchestrator/effect_runner.gleam` and remove daemon-local side-effect queue ownership.
- [ ] Milestone 2: extract workflow reload ownership into `src/scherzo/orchestrator/workflow_reloader.gleam`.
- [ ] Milestone 3: extract EventHub publishing helpers into `src/scherzo/orchestrator/event_publisher.gleam`.
- [ ] Milestone 4: extract worker, YAML run, and YAML step-command bookkeeping into `src/scherzo/orchestrator/worker_registry.gleam`.
- [ ] Milestone 5: extract poll and retry timer bookkeeping into scheduler modules.
- [ ] Milestone 6: extract operator/control command handling into `src/scherzo/orchestrator/control_command_handler.gleam`.
- [ ] Milestone 7: clean up the daemon state shape, run final validation, and write the retrospective.

## Surprises & Discoveries

- Observation: The current test suite intentionally prints Erlang `ERROR REPORT` blocks from tests that verify crashed workers are handled.
  Evidence: The baseline `direnv exec . gleam test` run printed crash reports from `test/orchestrator_daemon_test.gleam` and `test/workflow_run_test.gleam` but still ended with `377 passed, no failures`.

- Observation: `src/scherzo/orchestrator/core.gleam` is already a pure runtime-transition module and should not become the new god object.
  Evidence: It defines `core.Effect`, `core.Transition`, `core.new_state`, `core.apply_worker_success`, `core.apply_worker_failure`, `core.schedule_retry`, `core.reconcile_issue`, and invalid-workflow report state helpers, but it does not start processes, schedule timers, read files, or publish EventHub events.

- Observation: Some lifecycle decomposition has already happened outside the daemon.
  Evidence: `src/scherzo/orchestrator/service.gleam` already delegates graceful daemon stop behavior to `src/scherzo/lifecycle.gleam` and `src/scherzo/signal.gleam`. This plan should not reopen that work.

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

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo is a Gleam service that runs pi agents from Linear issues. The repository targets Erlang, as shown by `target = "erlang"` in `gleam.toml`. The main daemon implementation is `src/scherzo/orchestrator/daemon.gleam`. A daemon is a long-running process that polls Linear, claims issues, starts agent workers, reports results, serves a local control API, and publishes session events.

The daemon actor's public entry points are `daemon.start(workflow_path, dependencies)`, `daemon.shutdown(subject, timeout_ms)`, `daemon.get_snapshot(subject, timeout_ms)`, and `daemon.apply_operator_command(subject, operator_command, timeout_ms)`. The actor receives a single large `Message` type that currently includes poll ticks, retry ticks, worker completions, worker updates, worker command readiness, YAML step events, monitor down events, side-effect completions, shutdown, snapshot requests, and operator commands.

`src/scherzo/orchestrator/core.gleam` owns pure runtime transitions. It defines `core.Effect` values such as `Dispatch`, `ScheduleRetry`, `CancelRetry`, `CleanupWorkspace`, `ReleaseClaim`, `StopWorker`, and `ParkIssue`. The daemon currently interprets those effects by scheduling timers, starting workers, and enqueueing side effects.

A side effect in this plan means work that may block or fail outside pure state transitions: tracker fetches, Linear command fetches and acknowledgements, handoff claim/success/failure reports, invalid-workflow triage reports, and workspace cleanup. Today these are represented by the private `SideEffect` type in `daemon.gleam`, run by `run_side_effect`, and completed through the public `SideEffectResult` type. A side-effect runner actor is a separate process that owns the queue and starts monitored worker processes for those effects.

An EventHub is Scherzo's in-memory session event stream. The daemon currently publishes lifecycle events and pi updates directly through `scherzo/session/hub`. A worker registry is the runtime bookkeeping that maps issue ids, session ids, and process monitors to active legacy workers, YAML workflow runs, and YAML step command subjects.

The local control API is implemented in `src/scherzo/control/server.gleam`, `src/scherzo/control/command.gleam`, `src/scherzo/control/file.gleam`, and related modules. The daemon still owns the actor-side handling of those operator commands today.

## Preconditions and Verified Facts

The current tree has these relevant files:

- `src/scherzo/orchestrator/daemon.gleam`, verified at 4,184 lines with `wc -l`.
- `src/scherzo/orchestrator/core.gleam`, verified at 808 lines with `wc -l`.
- `src/scherzo/orchestrator/service.gleam`, verified at 1,181 lines with `wc -l`.
- `src/scherzo/control/linear_transport.gleam`, which already isolates Linear comment parsing, processed-comment state, and Linear command transport actions.
- `src/scherzo/tracker.gleam`, where `tracker.Client` is a record of tracker fetch functions.
- `src/scherzo/error.gleam`, where tracker and workspace error variants live.
- Daemon tests under `test/orchestrator_daemon_test.gleam`, `test/orchestrator_daemon_control_test.gleam`, `test/orchestrator_daemon_session_event_test.gleam`, and `test/orchestrator_daemon_linear_command_test.gleam`.

The current baseline command from the repository root is:

    direnv exec . gleam test

On 2026-04-30 it ended with:

    377 passed, no failures

The baseline also showed expected Erlang crash reports from tests that intentionally crash worker processes. Do not treat those reports as failures unless the final line reports failures or the command exits nonzero.

The current daemon side-effect implementation is in `src/scherzo/orchestrator/daemon.gleam` around the functions `enqueue_side_effect`, `drain_side_effects`, `max_side_effects`, `spawn_side_effect`, and `run_side_effect`. It uses `process.spawn_unlinked` for side-effect workers and increments `side_effects_in_flight` in the daemon state.

The current daemon worker implementation also uses `process.spawn_unlinked` in `spawn_worker`. This plan does not remove worker process spawning from the daemon in the first milestone; it only removes unmonitored side-effect queue ownership. Worker processes are already monitored through `worker_monitors` and `yaml_run_monitors`.

## Scope Boundaries

In scope: extracting the side-effect queue into a monitored runner actor; adding tests for side-effect crash recovery; moving workflow reload code into a workflow reloader module; moving EventHub event payload helpers into an event publisher module; moving worker/YAML run/step command dictionaries into a worker registry module; moving poll and retry timer bookkeeping into scheduler modules; moving operator command decision logic into a command handler module; simplifying `State` in `daemon.gleam`; preserving all existing external daemon behavior.

Out of scope: durable crash recovery after BEAM death, host restart, `kill -9`, or power loss; persisting side-effect queues; changing Linear command syntax; changing EventHub event shape; changing local control API routes or authentication; changing workflow YAML semantics; changing pi worker internals; converting Scherzo to a full OTP application or supervision tree; adding distributed daemon coordination.

The final top-level daemon actor may still start workers, decide when to dispatch, and bridge between subsystems. It should not own every subsystem's private dictionaries and helper functions directly.

## Milestones

Milestone 0 adds characterization tests and records the baseline. At the end, the suite still passes before any extraction, and there is at least one failing or skipped test that demonstrates the side-effect crash stall if run against the old implementation. If the crash test cannot be made to fail deterministically before the implementation, record why in Surprises & Discoveries and keep the effect-runner unit test as the primary proof.

Milestone 1 introduces `src/scherzo/orchestrator/effect_runner.gleam`. At the end, side-effect queue state and side-effect process monitoring live outside `daemon.gleam`. A crashing side-effect worker frees runner capacity, emits a crash completion, logs `side_effect_crashed`, and does not prevent later queued effects or later polls from running.

Milestone 2 introduces `src/scherzo/orchestrator/workflow_reloader.gleam`. At the end, file reads, bundle reload, reload validation, last-known-good reload state, workflow definition selection, and secret recomputation are owned by that module. The daemon only asks the reloader to reload and then rebuilds runtime clients when the effective config changes.

Milestone 3 introduces `src/scherzo/orchestrator/event_publisher.gleam`. At the end, `publish_worker_update`, `publish_lifecycle`, `update_payload`, event kind classification, status classification, blocking UI method classification, and token nonzero checks are no longer in `daemon.gleam`.

Milestone 4 introduces `src/scherzo/orchestrator/worker_registry.gleam`. At the end, active worker handles, YAML run handles, session lookup, monitor lookup, step command subject lookup, command-ready registration, route clearing, and registry cleanup are owned by the registry. The daemon asks the registry to register, find, remove, and resolve monitor-down events.

Milestone 5 introduces scheduler modules for poll and retry timer state. At the end, `poll_generation`, `poll_in_flight`, `poll_timer`, `retry_timers`, and `retry_refreshes_in_flight` are no longer top-level daemon state fields. The daemon still receives `PollTick` and `RetryTick`, but acceptance, staleness checks, rescheduling, canceling, and generation bookkeeping are delegated.

Milestone 6 introduces `src/scherzo/orchestrator/control_command_handler.gleam`. At the end, operator command cases such as pause, resume, reload, retry, park, unpark, abort, stop-after-current-turn, prompt, and UI response live in that module with explicit dependencies and callbacks. The daemon still receives `ApplyOperatorCommand` and sends the reply, but it does not contain the large command decision tree.

Milestone 7 removes dead daemon code and writes the retrospective. At the end, the daemon remains the public integration actor, all tests pass, structural grep checks pass, and this plan records the remaining gaps.

## Plan of Work

Begin with tests because this refactor changes concurrency ownership. Add the side-effect crash tests before moving the queue. Then introduce `effect_runner.gleam` with the same side-effect variants and result variants currently in `daemon.gleam`, but with queue state, in-flight tracking, and monitor handling internal to the runner actor. Wire the daemon to start the runner during actor initialization and enqueue effects through `effect_runner.enqueue` instead of mutating `side_effect_queue`.

After the side-effect runner is stable, move code that has minimal coupling. Workflow reload is a good next extraction because it mostly depends on `runtime_bundle`, `config`, `simplifile`, and `domain`. Event publishing is also low risk because it can accept an event hub subject, session id, and update payload without seeing daemon state.

Then move registry state. This is more coupled because worker finish, worker down, command routing, shutdown, and EventHub publishing interact. Keep the migration incremental: first move the handle types and lookup functions, then command-ready route management, then monitor-down resolution, then shutdown cleanup. Run the daemon tests after each slice.

Next move scheduler state. Start with poll generation and staleness because that is small, then retry timers and retry refresh in-flight tracking. Do not move dispatch policy from `core.gleam`; keep pure runtime decisions where they already are.

Extract control command handling last because it depends on all earlier boundaries. Its public function should take a context record or explicit callbacks instead of importing daemon state. The daemon should call it, receive a new context plus `command.CommandResult`, log the result, send the reply, and continue.

## Concrete Steps

1. From the repository root, run `git status --short` and confirm there are no unrelated changes. If there are unrelated changes, stop and either commit/stash them or record exactly why they are safe to keep.

2. From the repository root, run `direnv exec . gleam test`. Expect the suite to end with `377 passed, no failures` if no other tests have changed since this plan was written. If the count differs because other tests were added, accept any count only if the command exits zero and reports `no failures`.

3. Create `test/orchestrator_effect_runner_test.gleam`. Add a test named `effect_runner_runs_successful_effect_once_test`. It should start the new runner with `max_concurrent: 1`, enqueue a `CleanupWorkspace` effect whose cleanup function sends `"cleanup_started"` to a test subject and returns `Ok(Nil)`, and assert that the notification subject receives exactly one `Finished` completion and no second completion within a short timeout.

4. In the same test file, add `effect_runner_reports_crash_and_drains_queue_test`. It should enqueue a first `CleanupWorkspace` effect whose cleanup function panics with `panic as "boom"`, then enqueue a second `CleanupWorkspace` effect whose cleanup function returns `Ok(Nil)`. Assert that the notification subject receives one `Crashed` completion for the first effect and then one `Finished` completion for the second effect. This test proves a crash frees queue capacity.

5. In `test/orchestrator_daemon_test.gleam`, add an integration test named `daemon_side_effect_crash_does_not_stall_future_polls_test`. Use a fake `tracker.Client` whose first `fetch_candidate_issues` call panics and whose second call returns `Ok([])`. Start the daemon with `send_after` returning `daemon.TestTimer(delay)`, send `daemon.PollTick(1)`, wait for a `side_effect_crashed` log event, then send `daemon.PollTick(2)` and assert that the second fetch is observed. If the old daemon cannot make this test deterministic, mark the discovery in this plan and keep the effect-runner unit test as the deterministic crash proof.

6. Run `direnv exec . gleam test`. Before implementation, the new effect-runner tests will not compile because `src/scherzo/orchestrator/effect_runner.gleam` does not exist. This is the expected red phase.

7. Create `src/scherzo/orchestrator/effect_runner.gleam`. Define `pub type Effect` by moving the existing daemon-private side-effect variants: `FetchCandidates`, `FetchLinearCommands`, `RefreshRunning`, `RefreshRetry`, `ClaimIssue`, `ReportSuccess`, `ReportFailure`, `PostLinearCommandAck`, `ReportInvalidWorkflow`, and `CleanupWorkspace`. Define `pub type EffectResult` by moving the existing daemon `SideEffectResult` variants. Define `pub type Completion { Finished(id: Int, result: EffectResult) Crashed(id: Int, effect: Effect, reason: String) }`.

8. In `effect_runner.gleam`, define an actor-owned message type with `Enqueue(Effect)`, `WorkerFinished(Int, EffectResult)`, `WorkerDown(process.Down)`, and `Shutdown(process.Subject(Nil))`. Define a `Handle` type wrapping `process.Subject(Message)` and public functions `start`, `enqueue`, and `shutdown`.

9. In `effect_runner.gleam`, implement internal state with `next_id: Int`, `queue: List(QueuedEffect)`, `in_flight: Dict(Int, InFlightEffect)`, `monitors: Dict(process.Monitor, Int)`, `max_concurrent: Int`, `notify: fn(Completion) -> Nil`, and `logger: fn(String, String, List(log.Field)) -> Nil`. Keep `max_concurrent` configurable but use `4` from production daemon wiring to preserve current behavior.

10. Move `run_side_effect` logic from `daemon.gleam` into `effect_runner.gleam`. Keep the behavior of every successful side effect unchanged: tracker fetches call the same client functions, handoff calls the same client functions, invalid workflow reports call the same triage client, and cleanup calls the supplied cleanup function.

11. Implement runner draining so it starts queued effects while `dict.size(in_flight) < max_concurrent`. Each worker must be started with `process.spawn_unlinked`, monitored immediately, and tracked by id and monitor. On normal `WorkerFinished`, demonitor the process, remove the in-flight entry, call `notify(Finished(id, result))`, and drain again. On `WorkerDown` for a known monitor, remove the in-flight entry, call `notify(Crashed(id, effect, "process_down"))`, log `side_effect_crashed`, and drain again. Ignore stale monitor downs.

12. Update `src/scherzo/orchestrator/daemon.gleam` to import `scherzo/orchestrator/effect_runner`. Replace the daemon `SideEffect` and `SideEffectResult` types with uses of `effect_runner.Effect`, `effect_runner.EffectResult`, and `effect_runner.Completion`. Change the daemon message from `SideEffectFinished(SideEffectResult)` to `SideEffectCompleted(effect_runner.Completion)`.

13. In `daemon.start`, start an effect runner inside the actor initializer after the daemon subject is available. Pass `max_concurrent: 4`, a logger that calls the daemon logger with current secrets, and a notify function that sends `SideEffectCompleted(completion)` to the daemon subject. Store the returned `effect_runner.Handle` in daemon state.

14. Replace `enqueue_side_effect`, `drain_side_effects`, `max_side_effects`, `spawn_side_effect`, and `run_side_effect` in `daemon.gleam` with a small `enqueue_side_effect` wrapper that calls `effect_runner.enqueue(state.effect_runner, effect)` and returns `state`. Remove `side_effects_in_flight` and `side_effect_queue` from daemon `State`.

15. Replace `handle_side_effect_finished` with `handle_side_effect_completed`. For `Finished(_, result)`, call the existing result-specific handlers. For `Crashed(_, effect, reason)`, log `side_effect_crashed` with an effect kind and synthesize an error result of the same shape the old handler expects. Use `error.LinearApiRequest("side_effect_crashed")` for tracker-like effects and `error.WorkspaceIo("side_effect_crashed")` for cleanup. Then call the same result-specific handler so poll, retry, pending claim, invalid workflow, ack, and cleanup paths complete normally.

16. In `shutdown_state`, call `effect_runner.shutdown(state.effect_runner, 1000)` before clearing daemon runtime fields. The runner shutdown should kill or demonitor in-flight side-effect workers, drop queued effects, send the ack, and ignore any later stale completions.

17. Run `direnv exec . gleam format --check src test`, then `direnv exec . gleam test`. Expect all tests to pass. The pass count should be at least the baseline plus the new tests. Commit this milestone with a message such as `Extract monitored side-effect runner`.

18. Create `src/scherzo/orchestrator/workflow_reloader.gleam`. Move `workflow_definition_from_bundle`, `reload_if_changed`, `apply_new_contents`, `validate_reloaded_bundle`, `apply_reloaded_bundle`'s pure reload-state construction, and `mark_reload_invalid` logic into this module. Define a `State` containing `workflow_path`, `chosen_path`, `last_contents`, `bundle`, `definition`, `reload_state`, `effective`, and `secrets`.

19. In `workflow_reloader.gleam`, expose `from_bundle(workflow_path, bundle)`, `reload_if_changed(state)`, and `reload_now(state)`. Return an `Outcome` that distinguishes unchanged, reloaded, and invalid states. The module should not create tracker, handoff, Linear command, or triage clients; it only returns the new effective config and secrets so the daemon can rebuild clients.

20. Add `test/orchestrator_workflow_reloader_test.gleam`. Write one test that loads a workflow and confirms unchanged contents return an unchanged outcome. Write a second test that changes polling interval in the workflow file and confirms `reload_if_changed` returns a reloaded state with the new interval. Write a third test that writes invalid config and confirms the returned reload state is `config.CurrentInvalid` while the previous effective config remains the last known good config.

21. Update `daemon.gleam` so workflow-related fields are replaced by `workflow: workflow_reloader.State` where practical. If a single commit replacing all fields is too large, first call `workflow_reloader` functions while keeping the old fields, then collapse the fields in a second commit. Rebuild tracker, handoff, Linear command, and triage clients in the daemon only when the reloader returns a valid reloaded effective config.

22. Run format and tests. Commit this milestone with a message such as `Extract daemon workflow reloader`.

23. Create `src/scherzo/orchestrator/event_publisher.gleam`. Move `publish_worker_update`, `publish_lifecycle`, `update_payload`, `kind_for_update`, `pi_type_for_update`, `status_for_update`, `is_blocking_ui_method`, and `tokens_are_nonzero` into this module. Expose `worker_update(event_hub, session_id, update)` and `lifecycle(event_hub, session_id, name, message)`.

24. Add `test/orchestrator_event_publisher_test.gleam` or extend `test/session_event_test.gleam` with direct tests for event kind classification. Include at least one raw pi event, one blocking `extension_ui_request` with method `input`, one non-blocking UI method, one tool-shaped `message`, and one `turn_finished` token stats event.

25. Update `daemon.gleam` to call `event_publisher.worker_update` and `event_publisher.lifecycle`. Delete the moved helper functions from `daemon.gleam`.

26. Run format and tests. Commit this milestone with a message such as `Extract daemon event publisher`.

27. Create `src/scherzo/orchestrator/worker_registry.gleam`. Move `WorkerHandle` and `YamlRunHandle` into this module unless a public compatibility issue is found. Define `Registry` with workers, worker monitors, YAML runs, YAML run monitors, issue sessions, step command subjects, step command monitors, step command subject monitors, and next session sequence.

28. In `worker_registry.gleam`, implement `new`, `reserve_session_sequence`, `register_worker`, `register_yaml_run`, `register_worker_command_subject`, `register_yaml_step_command_subject`, `clear_yaml_step_command_route`, `worker_for_session`, `yaml_run_for_session`, `active_issue_ids`, `active_issues`, `has_active_run`, `remove_worker`, `remove_yaml_run`, `remove_all`, and `resolve_down`. `resolve_down` should return a value that tells the daemon whether a down monitor belonged to a legacy worker, YAML workflow run, YAML step command subject, or nothing known.

29. Add `test/orchestrator_worker_registry_test.gleam`. Cover registering a worker and looking it up by session id, registering a YAML run and resolving its monitor down, registering and clearing a YAML step command subject, and `remove_all` demonitoring or forgetting all entries without leaving session mappings behind.

30. Update `daemon.gleam` in small slices. First replace lookup helpers such as `worker_for_session`, `yaml_run_for_session`, `active_run_issue_ids`, `active_run_issues`, `has_active_run`, and first-worker helpers with registry calls. Run tests. Then replace command-ready route management. Run tests. Then replace monitor-down handling. Run tests. Then replace shutdown registry cleanup. Run tests.

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

    grep -n "side_effects_in_flight\|side_effect_queue\|fn run_side_effect\|fn spawn_side_effect" src/scherzo/orchestrator/daemon.gleam
    grep -n "fn update_payload\|fn kind_for_update\|fn publish_worker_update" src/scherzo/orchestrator/daemon.gleam
    wc -l src/scherzo/orchestrator/daemon.gleam

    The first two grep commands should print no matches. The line count should be substantially below 4,184; if it is still above roughly 2,500 lines, stop and record why the remaining code belongs in the daemon before calling this complete.

44. Run final validation:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

    Expect both commands to exit zero and the test command to report `no failures`. The exact passed count should be the baseline plus the tests added during this plan unless other concurrent work changes the count.

45. Update this plan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective. Commit the cleanup and plan update with a message such as `Document daemon decomposition outcome`.

## Testing and Falsifiability

The main falsifiable safety claim is: a side-effect process crash no longer stalls the side-effect queue or leaves the daemon poll phase stuck forever. The deterministic unit proof is `test/orchestrator_effect_runner_test.gleam` with `effect_runner_reports_crash_and_drains_queue_test`: the first effect panics, the second effect still runs, and the runner emits both a crash completion and a success completion. The daemon integration proof is `daemon_side_effect_crash_does_not_stall_future_polls_test`: after a crashing first candidate fetch, a later poll tick causes another fetch. If either test fails, the plan has not fixed the concrete concurrency bug.

The behavior-preservation claim is falsified by any regression in the existing suite. The most important parity tests are the daemon tests for dispatch, retries, YAML workflow sessions, Linear operator commands, session events, control server commands, lifecycle shutdown, and service lifecycle. Existing tests must keep their current assertions; do not weaken tests to make extraction easier.

The module-boundary claim is falsified if the final daemon still contains the side-effect queue implementation, event payload classification helpers, workflow reload implementation, retry timer dictionaries, worker registry dictionaries, and control command decision tree. The structural grep checks in Concrete Step 43 are mandatory. Line count alone is not acceptance, but a final daemon close to the original 4,184 lines means the extraction did not accomplish the maintainability goal.

For new tests, use these concrete scenarios:

- In `test/orchestrator_effect_runner_test.gleam`, assert a successful cleanup effect emits exactly one `Finished` completion and no duplicate completion after its process exits.
- In `test/orchestrator_effect_runner_test.gleam`, assert a panicking cleanup effect emits `Crashed`, then a queued cleanup effect emits `Finished`.
- In `test/orchestrator_workflow_reloader_test.gleam`, assert unchanged contents do not reload, valid changed contents reload and update `effective.polling.interval_ms`, and invalid contents mark `config.CurrentInvalid` without discarding the last known good config.
- In `test/orchestrator_event_publisher_test.gleam` or `test/session_event_test.gleam`, assert the moved event classifier still maps blocking UI requests to `session_event.UiRequest`, UI responses to `session_event.UiResponse`, tool-shaped messages to `session_event.Tool`, `turn_finished` to `session_event.TokenStats`, and raw unknown events to `session_event.PiRaw`.
- In `test/orchestrator_worker_registry_test.gleam`, assert worker registration, YAML run registration, monitor resolution, session lookup, and route clearing remove all relevant maps.
- In `test/orchestrator_poll_scheduler_test.gleam`, assert stale poll ticks are rejected and finishing a poll clears in-flight state and increments generation.
- In `test/orchestrator_retry_scheduler_test.gleam`, assert canceling one retry timer does not delete another issue's timer and stale retry refresh completions are ignored.
- In `test/orchestrator_control_command_handler_test.gleam`, assert prompt and UI response size guards reject oversized payloads, worker replies map to the same command statuses, issue identifier resolution rejects ambiguous matches, and remote tracker fetch errors produce the same rejection reasons as the current daemon.

## Validation and Acceptance

Acceptance requires all of the following from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Both commands must exit zero. `gleam test` must report `no failures`. Expected Erlang crash reports from tests that intentionally crash worker or side-effect processes are acceptable only when the final test summary is green.

Behavior acceptance requires `daemon.start`, `daemon.shutdown`, `daemon.get_snapshot`, and `daemon.apply_operator_command` to retain their existing callers. The service lifecycle tests must still prove daemon mode handles graceful SIGTERM through `service.start_daemon_with_lifecycle`. The control tests must still prove pause, resume, reload, retry, park, unpark, abort, prompt, and UI response commands work through the daemon actor and Linear command transport.

Structural acceptance requires these checks:

    grep -n "side_effects_in_flight\|side_effect_queue\|fn run_side_effect\|fn spawn_side_effect" src/scherzo/orchestrator/daemon.gleam
    grep -n "fn update_payload\|fn kind_for_update\|fn publish_worker_update" src/scherzo/orchestrator/daemon.gleam

Both commands must return no matches. `wc -l src/scherzo/orchestrator/daemon.gleam` must show a substantial reduction from 4,184 lines. If the final daemon remains above roughly 2,500 lines, the implementer must record in Outcomes & Retrospective which concerns still remain and why they were intentionally left there.

Safety acceptance requires the new side-effect crash tests to pass. The runner must not stall when a side-effect worker crashes. The daemon must log a crash and continue poll/retry flow instead of leaving an in-flight side effect counted forever.

## Rollout, Recovery, and Idempotence

This is an internal refactor with one bug fix. There is no data migration and no operator rollout switch. Each milestone should be committed only after tests pass so the change can be backed out one milestone at a time. If a late extraction causes confusing failures, revert the last milestone rather than debugging across several uncommitted moves.

The side-effect runner changes live concurrency behavior. If they cause production trouble, the rollback is to revert the `EffectRunner` milestone and return to the old daemon-local queue while investigating. That rollback reintroduces the known queue-stall risk, so keep the crash tests in the tree if possible and mark them pending only as a temporary measure if rollback is necessary.

Module extraction steps are idempotent in the sense that running format and tests repeatedly is safe. Test temporary files should remain under `test/tmp/...`, matching existing repository convention. Do not create new persistent files outside `src/`, `test/`, and this plan unless a milestone explicitly records why.

## Artifacts and Notes

Baseline command recorded during plan authoring:

    direnv exec . gleam test
    ...
    377 passed, no failures

Baseline size check recorded during plan authoring:

    wc -l src/scherzo/orchestrator/daemon.gleam src/scherzo/orchestrator/core.gleam src/scherzo/orchestrator/service.gleam
      4184 src/scherzo/orchestrator/daemon.gleam
       808 src/scherzo/orchestrator/core.gleam
      1181 src/scherzo/orchestrator/service.gleam
      6173 total

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

In `src/scherzo/orchestrator/workflow_reloader.gleam`, define a state type that owns workflow path, chosen path, last contents, bundle, definition, reload state, effective config, and secrets. Expose reload functions that return an outcome and do not create network clients.

In `src/scherzo/orchestrator/event_publisher.gleam`, expose only event publishing and event classification functions. It should import `scherzo/session/hub`, `scherzo/session/event`, `scherzo/agent/runner`, and `scherzo/domain`; it should not import `daemon.gleam`.

In `src/scherzo/orchestrator/worker_registry.gleam`, define `Registry`, `WorkerHandle`, `YamlRunHandle`, and monitor/session lookup functions. It may import `gleam/erlang/process`, `gleam/dict`, `scherzo/domain`, and `scherzo/agent/worker_command`; it should not import `daemon.gleam`.

In scheduler modules, keep timer state generic over the timer handle type or accept callbacks. Do not import `daemon.TimerHandle` from scheduler modules.

In `src/scherzo/orchestrator/control_command_handler.gleam`, define an explicit context and dependencies record. It may import `scherzo/control/command`, `scherzo/agent/worker_command`, `scherzo/domain`, `scherzo/tracker`, `scherzo/orchestrator/core`, `scherzo/orchestrator/worker_registry`, and `scherzo/orchestrator/workflow_reloader`. It must not import `scherzo/orchestrator/daemon`.
