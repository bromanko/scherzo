# Extract the orchestrator daemon into a pure transition kernel

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo operators rely on the orchestrator daemon to poll Linear, claim eligible issues, run pi workers, recover interrupted work, respond to operator commands, and keep the local ledger and live session UI consistent. Today most of that behavior is concentrated in `src/scherzo/orchestrator/daemon.gleam`, a file currently measured at 5,921 lines. This makes routine changes risky because an implementation agent has to understand startup recovery, polling, dispatch, retries, Linear command handling, worker process management, event publishing, YAML step sessions, cleanup, and side-effect execution at the same time.

After this change, an operator should observe the same daemon behavior and the same once-mode behavior, but maintainers should be able to inspect and test most daemon orchestration behavior as pure state transitions. A pure transition is a function that receives a message and a data-only state, then returns a new data-only state plus an ordered list of explicit effects to interpret. The actor shell remains responsible for BEAM process details such as timers, monitors, control server handles, worker process identifiers, and effect execution. The practical outcome is that future changes to dispatch, retry, recovery, operator-command behavior, and Linear command receipt handling can be reviewed in small modules and covered by fast tests without booting the daemon actor.

The maintainability outcome is falsifiable. At completion, `src/scherzo/orchestrator/daemon.gleam` must no longer contain the handler families mapped in Scope Boundaries except for thin shell adapters, the new transition tests must cover the listed daemon-test categories, and any remaining private daemon decision function in those categories must have a Decision Log entry explaining why it is shell-owned. A reviewer should be able to understand dispatch decisions in `src/scherzo/orchestrator/transitions/dispatch.gleam`, retry decisions in `src/scherzo/orchestrator/transitions/retry.gleam`, Linear command ordering in `src/scherzo/orchestrator/transitions/linear_commands.gleam`, and worker lifecycle decisions in `src/scherzo/orchestrator/transitions/worker.gleam` without reading worker process spawning, timer, or control server code.

## Problem Framing and Constraints

The current daemon is the main agent-change risk because it mixes decision logic with side effects. For example, the same module decides whether a poll tick is stale, reloads workflow configuration from disk, fetches candidate issues, records Linear command receipts, validates dispatch claims, appends ledger records, spawns workers, publishes session lifecycle events, schedules retry timers, handles worker-down monitor messages, and shuts down event infrastructure. A small behavioral edit can accidentally disturb ledger append ordering, pending claim cleanup, retry generation checks, or event publication order.

The repository already has a useful lower-level pure module in `src/scherzo/orchestrator/core.gleam`. It defines `core.Transition`, `core.Effect`, `core.new_state`, dispatch precondition helpers, retry helpers, worker success/failure transitions, blocked dependency report tracking, invalid workflow report tracking, and token accounting. The target refactor should build on that direction rather than replacing it. The goal is not to invent a new framework; it is to move the daemon's existing message-level orchestration into pure modules and keep the actor shell small.

This plan must preserve all user-visible behavior. The daemon should still expose the same public API in `src/scherzo/orchestrator/daemon.gleam`: `start`, `shutdown`, `get_snapshot`, and `apply_operator_command`. The service facade in `src/scherzo/orchestrator/service.gleam` should keep supporting daemon mode, once mode, doctor mode, pi probe mode, and Linear contract checks. The local ledger format, Linear command receipt semantics, control protocol, event hub protocol, and workflow execution behavior are not in scope for redesign.

The extraction must happen in small, green slices. Each slice should compile, format, and pass tests before the next slice starts. No milestone should require a temporary broken daemon. If an implementer discovers that a proposed pure boundary would require changing stored ledger records or Linear-visible behavior, stop and update this plan before continuing.

## Strategy Overview

The chosen strategy is an incremental strangler refactor. First introduce data-only effect types, pure transition state types, and tests for the two highest-risk boundaries: ledger-gated continuations and worker-registry synchronization. Then move one behavior family at a time from private daemon functions into modules under `src/scherzo/orchestrator/transitions/`, leaving thin compatibility wrappers in `daemon.gleam` until each family is fully covered by pure tests. Finally reduce `daemon.gleam` to an actor shell that converts BEAM-specific messages into transition messages, invokes `transition.handle`, interprets the returned effects, drains any follow-up transition messages produced by the interpreter, and sends replies.

The final module shape is:

    src/scherzo/orchestrator/daemon.gleam
    src/scherzo/orchestrator/transition.gleam
    src/scherzo/orchestrator/transition_types.gleam
    src/scherzo/orchestrator/transitions/polling.gleam
    src/scherzo/orchestrator/transitions/dispatch.gleam
    src/scherzo/orchestrator/transitions/retry.gleam
    src/scherzo/orchestrator/transitions/worker.gleam
    src/scherzo/orchestrator/transitions/operator.gleam
    src/scherzo/orchestrator/transitions/linear_commands.gleam
    src/scherzo/orchestrator/transitions/recovery.gleam
    src/scherzo/orchestrator/transitions/shutdown.gleam
    src/scherzo/orchestrator/effects/types.gleam
    src/scherzo/orchestrator/effects/interpreter.gleam

The extra `transition_types.gleam` file is deliberate. If `transition.gleam` defines all transition types and also imports `transitions/polling.gleam`, while `transitions/polling.gleam` imports those types, the modules form a cycle. `transition_types.gleam` avoids that cycle by owning the shared `State`, `Message`, `Outcome`, and pending-action types. `transition.gleam` becomes the small public facade that dispatches messages to the specialized `transitions/*` modules.

The actor shell owns process-specific resources. The pure transition state owns data needed to make decisions. The effect interpreter is the boundary between them. Effects must be data-only: they must not contain `tracker.Client`, `handoff.Client`, `linear.CommandClient`, `linear_triage.TriageClient`, `process.Pid`, `process.Monitor`, `process.Subject`, `process.Timer`, or dependency function closures. The interpreter injects those resources when running an effect.

Ledger-gated work has one protocol. A transition that needs durable ledger records before later work must emit `AppendLedger(..., ContinueWith(...))` and must not emit the gated effect in the same outcome. The interpreter appends synchronously, returns a follow-up pure message `LedgerAppendCompleted(correlation_id, continuation, result, now_ms)`, and the daemon drains that message through `transition.handle` before accepting the next external daemon message. Only the success transition for `SpawnClaimedWorker` emits `StartWorker`; only the success transition for `EnqueueLinearAck` emits an ack enqueue effect. Append failure transitions log the configured failure event, preserve or clear pending state according to the legacy behavior named in Plan of Work, and emit no gated effect. Effects with `StopBatchOnFailure` are interpreted left-to-right in the same effect batch; a failed append stops only the remaining effects in that batch. Effects with `ContinueRegardless` are attempted synchronously, log on failure, and never gate later effects.

Worker process state has one synchronization model. `transition_types.WorkerDirectory` is authoritative for logical facts: which issue and run are active, which session id represents the run, which command route ids are expected, which YAML step sessions are open, and whether the kernel believes a worker is starting, running, stopping, or finished. `worker_registry.Registry` remains authoritative for shell facts: PIDs, monitors, command subjects, and process-level stopped-run bookkeeping. Route ids are generated by the pure transition before shell work starts; the shell stores a map from route id to the real process subject when it receives a ready message. A worker spawn or monitor failure is represented as a follow-up pure message `WorkerStartRegistered(...)` or `WorkerStartFailed(...)`, so the kernel mirror can be repaired instead of silently diverging from the shell registry.

This is proportionate because the daemon already has lower-level pure transition code, existing scheduler and registry helper modules, and a broad test suite. A one-shot rewrite would be too risky, but extracting message families behind a stable facade lets each risk be tested and reviewed independently. Once-mode sharing is explicitly deferred from this ExecPlan: once mode must keep working, but adapting `src/scherzo/orchestrator/service.gleam` to use the new daemon transition model is a follow-up unless this plan is revised with concrete synchronous-interpreter requirements.

## Alternatives Considered

The simplest alternative is to split `daemon.gleam` mechanically by copying private functions into several modules while leaving the same side effects embedded in those functions. That would reduce file length, but it would not solve the main maintenance problem: tests would still need to boot actors or fake many side effects to verify dispatch, retry, or recovery behavior. It would also continue to hide ledger writes and timer scheduling inside control flow.

Another alternative is to expand `src/scherzo/orchestrator/core.gleam` until it contains all daemon decisions. This is too broad for one module. `core.gleam` already has 1,118 lines and is a good place for low-level runtime facts such as dispatch preconditions, retry backoff, worker success/failure transitions, parked issue state, blocked dependency report caches, invalid workflow report caches, and token accounting. It should remain a reusable domain kernel, not become the new daemon.

A third alternative is to introduce a feature flag and run the legacy daemon and new kernel side by side. That adds operational complexity without much benefit because this refactor should preserve internal behavior and local tests can characterize the transition outputs. Instead, use additive modules and compatibility wrappers during implementation, and keep the tree green at each commit.

## Risks and Countermeasures

The biggest risk is changing ordering around ledger appends. Some current flows continue even if a ledger append fails, while other flows intentionally stop. For example, retry scheduling updates runtime state and schedules a timer even though its ledger append is only logged on failure. By contrast, a successful handoff claim must append `WorkflowRunStarted`, `KnownWorkspace`, `RunStarted`, and `IssueCounterUpdated` before spawning the worker; if that append fails, the worker must not start. Linear command receipts also have strict ordering: `LinearCommandSeen` and `LinearCommandStarted` must be appended before applying the operator command, `LinearCommandCompleted` must be appended before enqueueing an ack, and `OutboxCompleted` plus `LinearCommandAcked` must be appended after the ack succeeds. The countermeasure is the ledger continuation protocol described in Strategy Overview and Plan of Work, plus interpreter tests with fake ledger success and failure. The plan is not complete until a test proves claim append success leads to `StartWorker`, claim append failure leads to no `StartWorker`, Linear command completion append failure leads to no ack enqueue, and retry append failure preserves the current continue-and-log behavior.

Another risk is losing process-level safety. The actor currently keeps real timer handles, process monitors, worker PIDs, command subjects, the effect runner handle, the event hub subject, and the control server handle. These must not move into pure state. The countermeasure is a strict actor shell boundary: transition state stores logical worker/session facts and pending work, while `daemon.gleam` and `effects/interpreter.gleam` store real BEAM resources. If a transition needs to learn whether shell work succeeded, the interpreter returns a follow-up pure message rather than mutating transition state directly.

Another risk is divergence between the pure worker mirror and `worker_registry.Registry`. A prompt, stop, abort, YAML step command, or worker-down event can be lost if the kernel knows about a route id that the shell never registered, or if the shell registry contains a PID that the kernel has already forgotten. The countermeasure is to generate route ids in pure state, use them as the only correlation key between transition effects and shell registry entries, and add invariant tests that interpret a `StartWorker` effect and then assert the same issue id, run id, session id, and route ids are present in both the `WorkerDirectory` and the registry projection. Spawn and monitor failures must return `WorkerStartFailed` and remove or mark the logical worker according to current failure handling.

Another risk is dispatching the same issue twice. Dispatch safety currently depends on `runtime.running`, `runtime.claimed`, `pending_claims`, `pending_dispatch_validations`, worker registry state, slot counts, and final claim validation after refreshing the issue from Linear. The countermeasure is to preserve `issue_is_running_claimed_or_pending`, `can_reserve_dispatch_slot`, final validation generation checks, and stale-result checks while moving them to pure transition modules. Add transition tests that cover active, claimed, pending, parked, stale validation, blocked dependency, and no-slot cases.

Another risk is breaking startup recovery. Startup recovery currently replays the ledger, fetches refreshed issue states, plans recovery, finalizes workflow candidates, appends recovery records, builds retry timers, queues cleanup, replays outbox acks, logs warnings, and resumes matching workflow checkpoints. The countermeasure is an explicit startup sequence: ledger replay, issue refresh for recovery, workflow finalization, and recovery/finalization ledger appends remain startup-fatal and happen before the actor begins accepting messages; applying an already-loaded recovery plan to schedule retry timers, enqueue cleanup, replay outbox acks, publish warnings, and request workflow resumption is pure and best-effort through effects after the actor shell exists. A failure in the startup-fatal portion returns `StartupError`; a failure in cleanup enqueueing, ack replay, warning logs, retry timer scheduling, or workflow resumption is logged or retried according to the existing effect semantics and does not retroactively fail startup.

Another risk is mishandling effect runner crashes. The current daemon treats `EffectRunnerDown(process.Down)` as a shell-level fatal event and shuts down abnormally after cleanup. That event contains process-specific data and must stay shell-owned. The countermeasure is to leave `EffectRunnerDown` in `daemon.Message` as a bypass path that does not call `transition.handle`; it calls the interpreter shutdown helper that mirrors current cleanup and then stops the actor abnormally. Existing daemon tests for effect-runner-down behavior must remain daemon or interpreter tests.

Another risk is making once mode diverge further from daemon mode. `src/scherzo/orchestrator/service.gleam` currently has a separate once-mode dispatch path starting at `run_once_with_dependencies`, `run_tick`, `dispatch_candidates`, `dispatch_issue`, and `execute_dispatch_issue`. It already shares some `core.gleam` functions with daemon mode, but it duplicates candidate selection, final validation, workflow routing, and effect interpretation. This ExecPlan does not solve once-mode sharing; it only requires no behavior regression. Deferring once-mode sharing keeps LIV-93 focused on extracting the daemon and avoids designing a synchronous interpreter in the middle of the high-risk daemon cutover.

## Progress

- [x] (2026-05-06 00:00Z) Read the ExecPlan authoring guidance in `.pi/skills/exec-plan/SKILL.md`.
- [x] (2026-05-06 00:00Z) Inspected the current orchestrator files, tests, and source-control status needed to draft this plan.
- [x] (2026-05-06 00:00Z) Drafted this ExecPlan as `docs/plans/LIV-93-extract-daemon-pure-transition-kernel.md`.
- [x] (2026-05-06 00:00Z) Incorporated adversarial review feedback covering ledger continuations, worker synchronization, interface closure, recovery semantics, effect-runner-down handling, once-mode deferral, and concrete-step granularity.
- [ ] Milestone 1: add closed transition/effect scaffolding and smoke tests with no behavior change.
- [ ] Milestone 2: prove ledger continuation and worker registry synchronization protocols with interpreter and shell tests.
- [ ] Milestone 3: extract polling, candidate dispatch, and retry transitions in wrapper-sized slices.
- [ ] Milestone 4: extract Linear command and operator command transitions in wrapper-sized slices.
- [ ] Milestone 5: extract worker lifecycle, YAML step session, and worker-down transitions in wrapper-sized slices.
- [ ] Milestone 6: extract startup recovery application and shutdown transitions.
- [ ] Milestone 7: reduce `daemon.gleam` to the actor shell through small interpreter cutover checkpoints.
- [ ] Milestone 8: remove legacy wrappers, update tests, and complete the retrospective. Once-mode transition sharing is deferred to a follow-up unless this plan is revised.

## Surprises & Discoveries

- Observation: The current daemon is slightly smaller than the ticket's approximate line count but still very large. It was measured at 5,921 lines with `wc -l src/scherzo/orchestrator/daemon.gleam`.
  Evidence: `wc -l` reported 5,921 lines for `src/scherzo/orchestrator/daemon.gleam`, 1,118 lines for `src/scherzo/orchestrator/core.gleam`, 479 lines for `src/scherzo/orchestrator/effect_runner.gleam`, and 1,837 lines for `src/scherzo/orchestrator/service.gleam`.
- Observation: Several helper modules already isolate useful pieces of the daemon and should be preserved rather than replaced.
  Evidence: `src/scherzo/orchestrator/core.gleam`, `src/scherzo/orchestrator/effect_runner.gleam`, `src/scherzo/orchestrator/worker_registry.gleam`, `src/scherzo/orchestrator/workflow_reloader.gleam`, `src/scherzo/orchestrator/poll_scheduler.gleam`, `src/scherzo/orchestrator/retry_scheduler.gleam`, `src/scherzo/orchestrator/control_command_handler.gleam`, and `src/scherzo/orchestrator/event_publisher.gleam` already exist and have focused tests.
- Observation: Once mode currently duplicates dispatch logic in `src/scherzo/orchestrator/service.gleam` rather than calling daemon message transitions.
  Evidence: `run_once_with_dependencies`, `run_tick`, `dispatch_candidates`, `validate_service_dispatch_issue`, `dispatch_issue`, `execute_dispatch_issue`, and `interpret_effects` are private functions in `src/scherzo/orchestrator/service.gleam`.

## Decision Log

- Decision: Keep `src/scherzo/orchestrator/core.gleam` as the low-level pure runtime module and build new message-level transition modules around it.
  Rationale: `core.gleam` already has pure functions and tests for dispatch preconditions, blocked dependencies, retry backoff, worker success/failure, parked issue handling, invalid workflow reports, and token accounting. Folding all daemon behavior into it would create another large module.
  Date: 2026-05-06.
- Decision: Add `src/scherzo/orchestrator/transition_types.gleam` in addition to `src/scherzo/orchestrator/transition.gleam`.
  Rationale: The facade must import specialized `transitions/*` modules, and those modules need shared state/message/outcome types. A separate types module avoids cyclic imports.
  Date: 2026-05-06.
- Decision: Effects returned by pure transitions must be data-only and must not carry clients, process handles, subjects, timers, or dependency closures.
  Rationale: Data-only effects are easy to assert in pure tests and make the actor shell boundary explicit. The interpreter can inject runtime dependencies when effects are executed.
  Date: 2026-05-06.
- Decision: Do not move all startup recovery I/O into pure transitions in the first slice.
  Rationale: Recovery reads and writes the ledger, fetches Linear issue state, uses artifact storage, and finalizes workflow candidates. The safe extraction is to keep I/O in the interpreter or shell while moving the deterministic application of the resulting recovery plan into `transitions/recovery.gleam`.
  Date: 2026-05-06.
- Decision: Use explicit ledger continuations rather than putting gated effects later in the same effect list.
  Rationale: A worker or Linear ack must not start unless the ledger records that gate it were appended successfully. `ContinueWith` plus a follow-up `LedgerAppendCompleted` message gives the kernel a single place to express success and failure behavior and makes append-gated tests observable.
  Date: 2026-05-06.
- Decision: Generate worker command and YAML route ids in pure state, while keeping real process subjects and monitors in `worker_registry.Registry`.
  Rationale: Route ids are data-only and testable. Keeping subjects, PIDs, and monitors shell-owned preserves the actor boundary and gives the interpreter a stable correlation key for shell registry updates.
  Date: 2026-05-06.
- Decision: Keep `EffectRunnerDown(process.Down)` as a shell-only fatal path instead of adding it to the pure transition message type.
  Rationale: The message contains process-specific data and current behavior is an actor supervision concern, not a business transition. Daemon or interpreter tests should preserve abnormal shutdown behavior.
  Date: 2026-05-06.
- Decision: Defer once-mode sharing to a follow-up unless stakeholders explicitly add it back with synchronous-interpreter acceptance criteria.
  Rationale: LIV-93 is about extracting the daemon. Once-mode sharing is desirable but would add a second effect interpretation model and extra parity surface after the daemon cutover risk is already high.
  Date: 2026-05-06.

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

This repository is a Gleam project. The package is named `scherzo` in `gleam.toml`, targets Erlang, and uses `gleeunit` for tests. From the repository root, the standard validation command is:

    direnv exec . gleam test

If `direnv exec . ...` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .` from the repository root, and retry the command through direnv. Formatting can be checked with:

    direnv exec . gleam format --check src test

The relevant orchestrator files currently are:

- `src/scherzo/orchestrator/daemon.gleam`: the current actor implementation. It defines public `StartupError`, public `Message`, `TimerHandle`, `ControlServerHandle`, `RuntimeDependencies`, a large private `State`, `default_dependencies`, `start`, `shutdown`, `get_snapshot`, `apply_operator_command`, and many private handlers. It owns process startup, control server startup, event hub startup, startup recovery, poll ticks, retry ticks, candidate dispatch, Linear command comment processing, operator command handling, pending claims, pending dispatch validations, worker process spawning, worker updates, YAML step sessions, effect runner completions, side-effect interpretation, ledger appends, shutdown, and logging.
- `src/scherzo/orchestrator/core.gleam`: a pure runtime domain module. It defines `core.Effect`, `core.Transition`, `core.new_state`, dispatch precondition helpers, workflow policy checks, active/terminal checks, worker success/failure transitions, retry scheduling, issue reconciliation, parked issue helpers, invalid workflow report helpers, blocked dependency report helpers, and token accounting. This module should remain and be called by the new transition modules.
- `src/scherzo/orchestrator/effect_runner.gleam`: an actor that runs asynchronous side effects with bounded concurrency. Its current `Effect` type includes values such as `FetchCandidates`, `FetchLinearCommands`, `RefreshRunning`, `RefreshRetry`, `ValidateDispatchClaim`, `ClaimIssue`, `ReportSuccess`, `ReportFailure`, `PostLinearCommandAck`, `ReportInvalidWorkflow`, and `CleanupWorkspace`. Today these effects carry clients and cleanup functions. In the target design, `effect_runner.gleam` remains a concurrency primitive, but its inputs should be adapted from data-only effects by `effects/interpreter.gleam`.
- `src/scherzo/orchestrator/state.gleam`: defines `RuntimeState`, `RunningEntry`, `RetryEntry`, `IssueCounter`, `ParkedEntry`, `InvalidWorkflowReport`, and `BlockedDependencyReport`. This state is already data-only and should remain part of the pure transition state.
- `src/scherzo/orchestrator/worker_registry.gleam`: tracks real worker handles, monitors, command subjects, session ids, YAML step command routes, and stopped YAML runs. Because it contains process PIDs, monitors, and subjects, the final pure transition state must not own this registry directly. Keep this module as the shell registry and mirror only data-only worker/session facts in transition state.
- `src/scherzo/orchestrator/poll_scheduler.gleam` and `src/scherzo/orchestrator/retry_scheduler.gleam`: currently parameterized by timer type and tested independently. They should remain helper modules, but pure transitions should use logical timer keys or timer metadata rather than real process timers.
- `src/scherzo/orchestrator/workflow_reloader.gleam`: stores the loaded workflow bundle, config reload state, effective config, and secrets. It also performs file reads in `reload_if_changed` and `reload_now`. Keep this module. Pure transitions may hold `workflow_reloader.State`, but file reads must be represented as effects and interpreted outside the pure transition.
- `src/scherzo/orchestrator/control_command_handler.gleam`: a generic helper for operator command application. It already receives callbacks and has focused tests. Keep it and use it from `transitions/operator.gleam`.
- `src/scherzo/orchestrator/event_publisher.gleam`: converts worker and lifecycle updates into session hub events. Keep it as an interpreter helper.
- `src/scherzo/orchestrator/service.gleam`: service entry points. `start_daemon` delegates to `daemon.start`; `run_once_with_dependencies` currently has a separate once-mode dispatch loop. This ExecPlan treats once mode as a no-regression surface: keep its existing tests passing, but do not adapt it to the new transition/effect model unless this plan is revised with synchronous-interpreter requirements.

The important test files currently include:

- `test/orchestrator_daemon_test.gleam`: broad daemon integration tests for dispatch, invalid workflow reporting, retry refresh, YAML workflows, side-effect crash handling, worker updates, retry timers, and startup recovery.
- `test/orchestrator_daemon_control_test.gleam`: control server and operator command tests.
- `test/orchestrator_daemon_linear_command_test.gleam`: Linear command receipt, ack, replay, and ordering tests.
- `test/orchestrator_daemon_session_event_test.gleam`: session event, worker update, worker down, retry session, recovery session, and event hub startup tests.
- `test/orchestrator_core_test.gleam`: pure tests around existing `core.gleam` behavior.
- `test/orchestrator_effect_runner_test.gleam`, `test/orchestrator_worker_registry_test.gleam`, `test/orchestrator_poll_scheduler_test.gleam`, `test/orchestrator_retry_scheduler_test.gleam`, and `test/orchestrator_service_test.gleam`: focused helper and service tests.

In this plan, “daemon mode” means the long-running actor started through `service.start_daemon` and `daemon.start`. “Once mode” means the single-pass path through `service.start_once` and `run_once_with_dependencies`. “Actor shell” means the small amount of code that deals with OTP actors, BEAM processes, process monitors, timers, control server handles, event hub subjects, and dependency injection. “Effect interpreter” means the module that receives data-only effects from pure transitions and performs actual I/O or process work.

## Preconditions and Verified Facts

The working copy was clean when this plan was drafted. Running:

    jj status --color=never

reported no changes.

The repository contains the orchestrator files named in this plan under `src/scherzo/orchestrator/`. No existing `docs/plans/LIV-93-*` file was found when the filename was chosen. The `test/` directory contains existing daemon, core, service, effect runner, worker registry, poll scheduler, retry scheduler, and Linear command tests that should be used as the safety net for this refactor.

The current `src/scherzo/orchestrator/daemon.gleam` public `Message` variants are `PollTick`, `RetryTick`, `WorkerFinished`, `WorkerUpdate`, `WorkerCommandReady`, `YamlStepStarted`, `YamlStepUpdate`, `YamlStepCommandReady`, `YamlStepFinished`, `AbortWorkerCommandTimedOut`, `WorkerDown`, `EffectRunnerDown`, `SideEffectCompleted`, `Shutdown`, `GetSnapshot`, and `ApplyOperatorCommand`. The target design may keep a shell-facing message type in `daemon.gleam`, but the pure transition message type must not expose process-specific variants such as `WorkerDown(process.Down)` or reply subjects.

The current `daemon.State` contains both pure data and shell resources. Pure data includes `workflow`, `linear_command_state`, `pending_linear_command_acks`, `in_flight_linear_command_acks`, `runtime`, `pending_claims`, `pending_dispatch_validations`, `next_dispatch_validation_generation`, `recovery_by_issue`, and `operator_paused`. Shell resources include `subject`, tracker/handoff/Linear clients, `poll` and `retry` scheduler states parameterized by `TimerHandle`, `registry` with PIDs and monitors, `effect_runner`, `effect_runner_monitor`, `event_hub`, `control_server`, `control_file_path`, and dependency closures.

The current `core.Effect` variants are `Dispatch`, `ScheduleRetry`, `CancelRetry`, `CleanupWorkspace`, `ReleaseClaim`, `StopWorker`, and `ParkIssue`. The current `effect_runner.Effect` variants are asynchronous operations for Linear/tracker/handoff/cleanup work and currently carry concrete clients or functions. The new effect vocabulary must unify these into data-only effects and interpreter-owned dependencies.

## Scope Boundaries

In scope:

- Add pure transition state, message, outcome, and effect types with closed constructors for the current daemon behavior.
- Add a ledger continuation protocol and interpreter follow-up message path before moving ledger-gated behavior.
- Add a pure `WorkerDirectory`, a shell registry correlation scheme, and tests proving the two cannot silently diverge after worker start, command route registration, YAML route registration, stop, finish, or down events.
- Add specialized transition modules for polling, dispatch, retry, worker lifecycle, operator commands, Linear commands, recovery application, and shutdown.
- Move deterministic decision logic out of `daemon.gleam` into those modules in wrapper-sized slices.
- Move side-effect interpretation out of `daemon.gleam` into `src/scherzo/orchestrator/effects/interpreter.gleam` in small effect-family checkpoints.
- Keep daemon public API compatibility.
- Preserve once-mode behavior and service tests. Do not redesign once mode in this plan.
- Add pure transition tests that replace broad daemon tests where possible.
- Add interpreter or daemon-shell tests for ledger-gated behavior, worker registry synchronization, and effect-runner-down handling.
- Keep daemon integration tests for actor startup, process supervision, control server, event hub, real timer, and worker process wiring.

Out of scope:

- Changing the local ledger record schema.
- Changing Linear command syntax, control protocol messages, or ack body format.
- Changing workflow YAML semantics.
- Replacing `effect_runner.gleam` with a different concurrency system.
- Changing the default concurrency or polling behavior.
- Adding new operator commands.
- Adapting once mode to share the transition/effect model. That is a follow-up ExecPlan unless this plan is revised with concrete synchronous interpreter semantics and parity tests.
- Changing once mode to run the full daemon polling/control/retry/Linear-command loop.

Responsibility mapping from the current daemon to target modules:

- Startup actor initialization, OTP selector setup, subject replies, effect runner start/monitor, effect-runner-down fatal handling, event hub start/stop, control server start/stop, process timers, worker PIDs, monitors, command subjects, shell route maps, and dependency closures stay in `src/scherzo/orchestrator/daemon.gleam` and `src/scherzo/orchestrator/effects/interpreter.gleam`.
- `load_startup_recovery`, ledger replay, issue-state fetch for recovery, workflow recovery finalization, and recovery/finalization ledger append stay startup-fatal and effectful. Their deterministic results are applied by `src/scherzo/orchestrator/transitions/recovery.gleam` after startup state exists.
- `handle_poll_tick`, stale poll generation checks, running refresh result handling, candidate fetch result handling, Linear command fetch phase sequencing, pending ack retry at poll end, candidate phase completion, and next-poll scheduling move to `src/scherzo/orchestrator/transitions/polling.gleam`.
- `dispatch_candidates`, blocked dependency reporting decisions, invalid workflow candidate decisions, workflow route checks, dispatch validation start/result handling, final validation preconditions, pending claim creation, slot accounting, run id/session sequence selection, and continuation dispatch move to `src/scherzo/orchestrator/transitions/dispatch.gleam`.
- `handle_retry_tick`, retry refresh start/result handling, retry candidate evaluation, stale generation handling, no-slot backoff, and retry timer cancellation/scheduling decisions move to `src/scherzo/orchestrator/transitions/retry.gleam`.
- `handle_worker_finished`, worker success/failure classification, handoff success/failure effect decisions, cleanup effect decisions, session event effect decisions, worker-down resolution after the shell converts `process.Down` into a logical event, YAML step started/finished/routing decisions, and step session cleanup move to `src/scherzo/orchestrator/transitions/worker.gleam`.
- `handle_operator_command`, pause/resume state changes, reload command decisions, retry/park/unpark command decisions, command-result shaping, and worker-command routing requests move to `src/scherzo/orchestrator/transitions/operator.gleam`, continuing to reuse `src/scherzo/orchestrator/control_command_handler.gleam`.
- `process_linear_command_comments`, `apply_linear_transport_actions`, `apply_linear_submit_command`, pending ack bookkeeping, ack retry enqueueing, safe command excerpt logic, command receipt ledger effects, and outbox replay state updates move to `src/scherzo/orchestrator/transitions/linear_commands.gleam`.
- `schedule_recovered_retry_timers`, `enqueue_recovered_cleanups`, `enqueue_startup_recovery_outbox`, recovery warning logs, and recovered workflow resumption requests move to `src/scherzo/orchestrator/transitions/recovery.gleam` after the effectful recovery plan has been loaded.
- `shutdown_state`, `shutdown_state_after_effect_runner_down`, `append_shutdown_step_attempt_interruptions`, `shutdown_step_attempt_interruption_bodies`, `shutdown_state_internal`, logical timer cancellation, stop-worker requests, session finish requests, shutdown interruption ledger requests, and event hub shutdown request creation move to `src/scherzo/orchestrator/transitions/shutdown.gleam` or shell-specific interpreter helpers according to whether they need process-specific resources.
- `append_ledger_bodies`, `apply_effect`, `apply_effects`, timer `send_after`/cancel calls, event hub operations, control file removal, worker process spawn, cleanup execution, tracker/handoff/triage/Linear calls, and effect runner enqueueing move to `src/scherzo/orchestrator/effects/interpreter.gleam`.

## Milestones

Milestone 1 establishes the new vocabulary without changing behavior. At the end of this milestone, the repository has `effects/types.gleam`, `transition_types.gleam`, `transition.gleam`, and initial pure transition tests that can assert effect lists. `daemon.gleam` still owns behavior through wrappers, so the daemon remains stable. This milestone proves that the proposed state and effect boundary can be represented in Gleam without import cycles.

Milestone 2 proves the two dangerous shell boundaries before broad extraction. At the end of this milestone, the interpreter has a small tested ledger-append continuation path, the daemon can drain follow-up pure messages returned by the interpreter, and worker start/route/down synchronization has a tested correlation model. This milestone exists so later dispatch and Linear command extraction cannot accidentally start workers or send acks before durable records exist.

Milestone 3 extracts polling, candidate dispatch, and retry transitions. At the end of this milestone, the logic that decides what happens after `PollTick`, candidate fetch completion, running refresh completion, retry tick, retry refresh completion, dispatch validation completion, and handoff claim completion lives outside `daemon.gleam`. The daemon still interprets effects, but pure tests verify stale generation handling, pause/no-slot behavior, dispatch validation, blocked dependency behavior, claim ledger gating, and retry backoff.

Milestone 4 extracts Linear command and operator command transitions. At the end of this milestone, command receipt ordering, ack enqueue/retry/replay decisions, pause/resume/retry/park/unpark decisions, and command-result creation are pure. Daemon tests remain for control-server authentication and actor reply delivery, while new transition and interpreter tests cover command state changes, ledger-gated command application, and ack effects.

Milestone 5 extracts worker lifecycle and YAML step-session transitions. At the end of this milestone, worker success/failure/down handling, stop/abort decisions, worker command route readiness, session event requests, and YAML step session route cleanup are expressed as transition outcomes. The interpreter still spawns and stops processes. Pure tests cover race-prone decisions; daemon integration tests cover actual process wiring.

Milestone 6 extracts recovery application and shutdown transitions. At the end of this milestone, startup recovery I/O still happens outside pure transitions, but applying the loaded recovery plan returns explicit effects for retry timers, cleanup, outbox ack replay, workflow resumption, warning logs, and session recovery lifecycle. Shutdown returns explicit effects for stopping workers, cancelling timers, appending interruption records after projection loading, stopping the control plane, and stopping the event hub.

Milestone 7 reduces `daemon.gleam` to the actor shell through small checkpoints instead of one large cutover. At the end of this milestone, `daemon.handle_message` maps shell messages to pure messages, calls `transition.handle`, passes effects to `effects/interpreter.apply`, drains returned follow-up messages, and sends any reply values. The effect interpreter owns side-effect execution and shell resource maps. `EffectRunnerDown(process.Down)` remains a shell-only bypass.

Milestone 8 removes legacy wrappers and finalizes tests. At the end of this milestone, broad daemon tests that only asserted pure decisions have moved down to transition tests, remaining daemon tests focus on shell behavior, all validation commands pass, and this plan's retrospective describes the final module sizes, test coverage changes, any intentionally retained daemon decision functions, and any once-mode follow-up work.

## Plan of Work

Start by defining the shared data model. In `src/scherzo/orchestrator/effects/types.gleam`, create data-only effect types. The names below are prescriptive. Do not add a field to these types merely for convenience; if current daemon behavior requires an additional data field, add it with a comment naming the current daemon function that needed it and record the reason in Decision Log.

    pub type Effect {
      Log(level: String, event: String, fields: List(log.Field))
      AppendLedger(request: LedgerAppend)
      SchedulePoll(delay_ms: Int, generation: Int)
      CancelPoll(generation: Int)
      ScheduleRetry(issue_id: String, delay_ms: Int, generation: Int, reason: reason.RetryReason)
      CancelRetry(issue_id: String, generation: Int, reason: String)
      Async(effect: AsyncEffect)
      StartWorker(request: WorkerStart)
      StopWorker(request: WorkerStop)
      PublishSession(effect: SessionEffect)
      Control(effect: ControlEffect)
      Shutdown(effect: ShutdownEffect)
      Reply(reply: ReplyEffect)
    }

    pub type LedgerAppend {
      LedgerAppend(
        correlation_id: String,
        bodies: List(record.RecordBody),
        failure_event: String,
        policy: LedgerPolicy,
      )
    }

    pub type LedgerPolicy {
      ContinueRegardless
      StopBatchOnFailure
      ContinueWith(continuation: LedgerContinuation)
    }

    pub type LedgerContinuation {
      NoLedgerContinuation
      SpawnClaimedWorker(issue_id: String, run_id: String, session_id: String)
      ApplyLinearCommand(comment_id: String)
      EnqueueLinearAck(comment_id: String)
      FinishShutdownInterruptions(shutdown_id: String)
    }

The ledger interpreter protocol is part of the interface. The interpreter handles `AppendLedger` synchronously and returns `transition_types.Message` values in its `ApplyResult.follow_up_messages`. For `ContinueWith(SpawnClaimedWorker(...))`, the original effect list must not contain `StartWorker`; the `LedgerAppendCompleted` success transition emits `StartWorker`, and the failure transition emits only a log effect and clears the matching pending claim if current daemon behavior clears it. For `ContinueWith(ApplyLinearCommand(...))`, success applies the command transition; failure logs and leaves the command receipt eligible for retry or replay without acking it. For `ContinueWith(EnqueueLinearAck(...))`, success records or keeps the pending ack and emits `Async(PostLinearCommandAck(...))`; failure logs and emits no ack. For retry ledger appends that current behavior treats as best effort, use `ContinueRegardless`: append failure logs but does not cancel the scheduled retry or stop later non-gated effects. For shutdown interruption appends, use `StopBatchOnFailure` if current behavior stops the remaining shutdown ledger batch, otherwise use `ContinueRegardless` and record the observed legacy behavior in Decision Log.

Define asynchronous effect and result values in the same file. These effects are requests for work; the existing `src/scherzo/orchestrator/effect_runner.gleam` remains the concurrency primitive and receives adapted shell effects from the interpreter.

    pub type AsyncEffect {
      FetchCandidates(generation: Int)
      FetchLinearCommands(generation: Int, issue_ids: List(String), candidates: List(tracker_issue.Issue), dispatch_after: Bool, limit_per_issue: Int)
      RefreshRunning(generation: Int, ids: List(String))
      RefreshRetry(issue_id: String, generation: Int)
      ValidateDispatchClaim(issue_id: String, generation: Int)
      ClaimIssue(issue: tracker_issue.Issue, workspace_path: String, run_id: String)
      ReportSuccess(issue_id: String, issue: tracker_issue.Issue, success: agent_types.WorkerSuccess, run_id: String)
      ReportFailure(issue_id: String, issue: tracker_issue.Issue, failure: agent_types.WorkerFailure, run_id: String)
      PostLinearCommandAck(issue_id: String, source_comment_id: String, body: String)
      ReportInvalidWorkflow(issue: tracker_issue.Issue, violation: workflow_policy.IssueWorkflowViolation, violation_fingerprint: String, reporting_policy_fingerprint: String)
      CleanupWorkspace(workspace_path: String)
      ReloadWorkflow(chosen_path: String)
      LoadShutdownProjection(shutdown_id: String, workspace_root: String, run_ids: List(String))
    }

    pub type AsyncResult {
      CandidateFetchFinished(generation: Int, result: Result(List(tracker_issue.Issue), error.TrackerError))
      LinearCommandFetchFinished(generation: Int, candidates: List(tracker_issue.Issue), dispatch_after: Bool, result: Result(List(linear.LinearComment), error.TrackerError))
      RunningRefreshFinished(generation: Int, result: Result(List(tracker_issue.Issue), error.TrackerError))
      RetryRefreshFinished(issue_id: String, generation: Int, result: Result(List(tracker_issue.Issue), error.TrackerError))
      DispatchClaimValidationFinished(issue_id: String, generation: Int, result: Result(tracker_issue.Issue, DispatchClaimValidationError))
      HandoffClaimFinished(issue_id: String, run_id: String, result: Result(Nil, error.TrackerError))
      HandoffSuccessFinished(issue_id: String, run_id: String, result: Result(Nil, error.TrackerError))
      HandoffFailureFinished(issue_id: String, run_id: String, result: Result(Nil, error.TrackerError))
      LinearCommandAckFinished(issue_id: String, source_comment_id: String, result: Result(Nil, error.TrackerError))
      InvalidWorkflowReportFinished(issue_id: String, violation_fingerprint: String, reporting_policy_fingerprint: String, result: Result(linear_triage.InvalidWorkflowReportOutcome, error.TrackerError))
      CleanupFinished(workspace_path: String, result: Result(Nil, error.WorkspaceError))
      WorkflowReloadFinished(result: workflow_reloader.Outcome)
      ShutdownProjectionLoaded(shutdown_id: String, result: Result(projection.Projection, ledger.LedgerError))
    }

Define the shell-facing effect families with closed constructors:

    pub type WorkerStart {
      WorkerStart(
        issue_id: String,
        run_id: String,
        session_id: String,
        command_route_id: String,
        issue: tracker_issue.Issue,
        workspace_path: String,
        workflow_id: String,
        route_label: String,
        recovery: Option(session_event.RecoveryInfo),
      )
    }

    pub type WorkerStop {
      WorkerStop(issue_id: String, run_id: String, session_id: String, reason: String)
      AbortWorker(issue_id: String, run_id: String, session_id: String, reason: String, timeout_ms: Int)
      SendWorkerCommand(route_id: String, command: command.WorkerCommand)
    }

    pub type SessionEffect {
      WorkerSessionStarted(session_id: String, issue_id: String, run_id: String, display_name: String, recovery: Option(session_event.RecoveryInfo))
      WorkerSessionUpdated(session_id: String, update: agent_types.RunnerUpdate)
      WorkerSessionFinished(session_id: String, issue_id: String, run_id: String, outcome: SessionOutcome)
      YamlStepSessionStarted(session_id: String, parent_session_id: String, run_id: String, display_name: String)
      YamlStepSessionUpdated(session_id: String, update: agent_types.RunnerUpdate)
      YamlStepSessionFinished(session_id: String, run_id: String, outcome: SessionOutcome)
      RecoveryWarning(session_id: String, issue_id: String, message: String)
    }

    pub type SessionOutcome {
      SessionSucceeded
      SessionFailed(reason: String)
      SessionStopped(reason: String)
      SessionRecovered
    }

    pub type ControlEffect {
      StopControlServer
      RemoveControlFile(path: String)
      WriteControlFile(path: String, contents: String)
    }

    pub type ShutdownEffect {
      BeginShutdown(shutdown_id: String)
      FinishShutdown(shutdown_id: String)
      StopEventHub(timeout_ms: Int)
      StopEffectRunner
    }

    pub type ReplyEffect {
      OperatorCommandReply(reply_id: String, result: command.CommandResult)
      SnapshotReply(reply_id: String, snapshot: orchestrator_state.RuntimeState)
      ShutdownReply(reply_id: String)
    }

`WorkerStart` intentionally does not contain a PID, monitor, subject, client, runtime dependency closure, or timer. If the existing worker-spawn call needs additional data such as workflow secrets or a DAG descriptor, add only the current data value needed to reproduce the existing spawn call, keep it data-only, and name that existing spawn parameter in Decision Log. `WorkerStop.SendWorkerCommand` uses a route id, not a process subject; the shell registry resolves the route id to the real subject.

In `src/scherzo/orchestrator/transition_types.gleam`, define the pure transition types. Start with the current daemon private pending data, but remove process-specific fields.

    pub type State {
      State(
        workflow: workflow_reloader.State,
        linear_command_state: linear_transport.TransportState,
        pending_linear_command_acks: Dict(String, PendingLinearCommandAck),
        in_flight_linear_command_acks: Dict(String, Bool),
        runtime: orchestrator_state.RuntimeState,
        poll: PollState,
        retry: RetryState,
        workers: WorkerDirectory,
        pending_claims: Dict(String, PendingClaim),
        pending_dispatch_validations: Dict(String, PendingDispatchValidation),
        next_dispatch_validation_generation: Int,
        next_session_sequence: Int,
        recovery_by_issue: Dict(String, session_event.RecoveryInfo),
        operator_paused: Bool,
      )
    }

    pub type Message {
      PollTick(generation: Int, now_ms: Int)
      RetryTick(issue_id: String, generation: Int, now_ms: Int)
      AsyncCompleted(result: effects_types.AsyncResult, now_ms: Int)
      LedgerAppendCompleted(correlation_id: String, continuation: effects_types.LedgerContinuation, result: Result(Nil, ledger.LedgerError), now_ms: Int)
      WorkerStartRegistered(issue_id: String, run_id: String, session_id: String, command_route_id: String, now_ms: Int)
      WorkerStartFailed(issue_id: String, run_id: String, session_id: String, reason: String, now_ms: Int)
      WorkerFinished(issue_id: String, run_id: String, result: Result(agent_types.WorkerSuccess, agent_types.WorkerFailure), now_ms: Int)
      WorkerUpdate(issue_id: String, update: agent_types.RunnerUpdate, now_ms: Int)
      WorkerCommandReady(issue_id: String, run_id: String, route_id: String)
      YamlStepStarted(session_id: String, parent_session_id: String, run_id: String, route_id: String, now_ms: Int)
      YamlStepUpdate(session_id: String, update: agent_types.RunnerUpdate, now_ms: Int)
      YamlStepCommandReady(session_id: String, route_id: String)
      YamlStepFinished(session_id: String, now_ms: Int)
      WorkerDown(event: WorkerDownEvent, now_ms: Int)
      OperatorCommand(reply_id: String, command: command.OperatorCommand, timeout_ms: Int, now_ms: Int)
      AbortWorkerCommandTimedOut(reply_id: String, command: command.OperatorCommand, session_id: String, now_ms: Int)
      StartupRecoveryLoaded(recovery: RecoveryStartup, now_ms: Int)
      ShutdownRequested(reply_id: String, now_ms: Int)
      SnapshotRequested(reply_id: String, now_ms: Int)
    }

    pub type Outcome {
      Outcome(state: State, effects: List(effects_types.Effect))
    }

`WorkerDirectory` is the pure mirror of worker/session facts that transitions need. Define it with these records:

    pub type WorkerDirectory {
      WorkerDirectory(
        by_issue: Dict(String, WorkerEntry),
        by_session: Dict(String, String),
        route_to_session: Dict(String, String),
        yaml_steps: Dict(String, YamlStepEntry),
        stopped_yaml_runs: Dict(String, String),
        next_route_sequence: Int,
      )
    }

    pub type WorkerEntry {
      WorkerEntry(
        issue_id: String,
        run_id: String,
        session_id: String,
        issue: tracker_issue.Issue,
        workspace_path: String,
        workflow_id: String,
        command_route_id: String,
        status: WorkerStatus,
        recovery: Option(session_event.RecoveryInfo),
      )
    }

    pub type WorkerStatus {
      WorkerStarting
      WorkerRunning
      WorkerStopping(reason: String)
      WorkerFinished
    }

    pub type YamlStepEntry {
      YamlStepEntry(
        session_id: String,
        parent_session_id: String,
        run_id: String,
        command_route_id: String,
        status: YamlStepStatus,
      )
    }

    pub type YamlStepStatus {
      YamlStepRunning
      YamlStepFinished
    }

    pub type WorkerDownEvent {
      WorkerDownEvent(issue_id: String, run_id: String, session_id: String, reason: String)
      UnknownWorkerDown(reason: String)
    }

    pub type PendingClaim {
      PendingClaim(issue_id: String, run_id: String, session_id: String, workspace_path: String, workflow_id: String, remaining_candidates: List(tracker_issue.Issue))
    }

    pub type PendingDispatchValidation {
      PendingDispatchValidation(issue_id: String, generation: Int, candidate: tracker_issue.Issue, remaining_candidates: List(tracker_issue.Issue))
    }

    pub type PendingLinearCommandAck {
      PendingLinearCommandAck(issue_id: String, source_comment_id: String, body: String, attempts: Int, last_error: Option(String))
    }

    pub type RecoveryStartup {
      RecoveryStartup(retries: List(RecoveredRetry), cleanups: List(RecoveredCleanup), outbox_acks: List(PendingLinearCommandAck), warnings: List(RecoveryWarning), workers: List(RecoveredWorker))
    }

    pub type RecoveredRetry {
      RecoveredRetry(issue_id: String, generation: Int, delay_ms: Int, reason: reason.RetryReason)
    }

    pub type RecoveredCleanup {
      RecoveredCleanup(workspace_path: String)
    }

    pub type RecoveryWarning {
      RecoveryWarning(issue_id: String, message: String)
    }

    pub type RecoveredWorker {
      RecoveredWorker(issue_id: String, run_id: String, session_id: String, workspace_path: String, recovery: session_event.RecoveryInfo)
    }

The route id scheme is deterministic and data-only. Use `worker:` plus `run_id` plus `:` plus the worker route sequence for primary worker command routes, and `yaml:` plus `run_id` plus `:` plus the YAML session id plus `:` plus the route sequence for YAML step command routes. The pure state increments `next_route_sequence` whenever it allocates a route id. The shell never invents route ids; it only records subjects against ids received from pure effects or pure messages.

`PollState` and `RetryState` are data-only:

    pub type PollState {
      PollState(
        scheduled_generation: Int,
        in_flight_generation: Option(Int),
        next_generation: Int,
        paused_since_ms: Option(Int),
      )
    }

    pub type RetryState {
      RetryState(
        scheduled: Dict(String, RetryTimerState),
        in_flight: Dict(String, Int),
        next_generation: Int,
      )
    }

    pub type RetryTimerState {
      RetryTimerState(issue_id: String, generation: Int, due_at_ms: Int, reason: reason.RetryReason)
    }

The actor shell or interpreter owns actual timer handles keyed by `poll` or by `issue_id` plus generation. Pure transitions only produce `SchedulePoll`, `CancelPoll`, `ScheduleRetry`, and `CancelRetry` effects.

In `src/scherzo/orchestrator/transition.gleam`, expose:

    pub fn initial_state(workflow_path: Option(String), bundle: runtime_bundle.RuntimeBundle, recovery: transition_types.RecoveryStartup) -> transition_types.State

    pub fn handle(message: transition_types.Message, state: transition_types.State) -> transition_types.Outcome

    pub fn snapshot(state: transition_types.State) -> orchestrator_state.RuntimeState

The `handle` function should be a dispatcher. It should call `transitions/polling.gleam` for poll and poll-related async results, `transitions/retry.gleam` for retry ticks/results, `transitions/dispatch.gleam` for dispatch validation and claim results, `transitions/linear_commands.gleam` for Linear command fetch and ack results, `transitions/worker.gleam` for worker and YAML messages, `transitions/operator.gleam` for operator commands, `transitions/recovery.gleam` for startup recovery application, and `transitions/shutdown.gleam` for shutdown and snapshot requests. If a message crosses domains, keep the top-level `handle` dispatch simple and let the domain module call another pure module explicitly.

The interpreter in `src/scherzo/orchestrator/effects/interpreter.gleam` should define shell-owned state and a single application function. It may call the existing `effect_runner.gleam` actor for asynchronous operations.

    pub type Clients {
      Clients(
        tracker_client: tracker.Client,
        handoff_client: handoff.Client,
        linear_command_client: linear.CommandClient,
        triage_client: linear_triage.TriageClient,
      )
    }

    pub type ShellState {
      ShellState(
        subject: process.Subject(daemon.Message),
        dependencies: daemon.RuntimeDependencies,
        clients: Clients,
        poll_timer: Option(daemon.TimerHandle),
        retry_timers: Dict(String, daemon.TimerHandle),
        registry: worker_registry.Registry,
        route_subjects: Dict(String, WorkerRouteSubject),
        pending_operator_replies: Dict(String, process.Subject(command.CommandResult)),
        pending_snapshot_replies: Dict(String, process.Subject(orchestrator_state.RuntimeState)),
        pending_shutdown_replies: Dict(String, process.Subject(Nil)),
        effect_runner: effect_runner.Handle,
        effect_runner_monitor: process.Monitor,
        event_hub: process.Subject(hub.Message),
        control_server: daemon.ControlServerHandle,
        control_file_path: Option(String),
      )
    }

    pub type WorkerRouteSubject {
      WorkerCommandSubject(process.Subject(command.WorkerCommand))
      YamlStepCommandSubject(process.Subject(command.WorkerCommand))
    }

    pub type ApplyResult {
      ApplyResult(
        shell: ShellState,
        follow_up_messages: List(transition_types.Message),
        outbound_replies: List(OutboundReply),
      )
    }

    pub type OutboundReply {
      OperatorCommandReply(reply_id: String, result: command.CommandResult)
      SnapshotReply(reply_id: String, snapshot: orchestrator_state.RuntimeState)
      ShutdownReply(reply_id: String)
    }

    pub fn apply(shell: ShellState, transition_state: transition_types.State, effects: List(effects_types.Effect)) -> ApplyResult

`OutboundReply` is data-only and contains correlation ids. Real reply subjects live only in `ShellState` maps. After `apply` returns, `daemon.gleam` sends each reply to the stored subject and removes that reply id from the matching map.

The daemon event loop must drain follow-up messages deterministically. For one incoming shell message, it should convert the shell message to zero or one pure message, call `transition.handle`, call `interpreter.apply`, send returned replies, then process `ApplyResult.follow_up_messages` in order by repeating the same transition-and-apply loop. Stop draining only when no follow-up messages remain. This is how ledger append completions and worker start registration repair pure state before the next external message is handled.

## Concrete Steps

1. From the repository root, confirm the workspace is clean:

       jj status --color=never

   Expect output indicating no changes, or only the plan file if implementation has already begun updating this living document.

2. Run the current test suite to capture the baseline before implementation:

       direnv exec . gleam test

   Expect all existing tests to pass. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry. Record any unexpected failure in Surprises & Discoveries before changing code.

3. Create `src/scherzo/orchestrator/effects/types.gleam` with the `Effect`, `LedgerAppend`, `LedgerPolicy`, `LedgerContinuation`, `AsyncEffect`, `AsyncResult`, `WorkerStart`, `WorkerStop`, `SessionEffect`, `ControlEffect`, `ShutdownEffect`, and `ReplyEffect` constructors specified in Plan of Work. Do not edit `daemon.gleam` in this step.

4. Create `src/scherzo/orchestrator/transition_types.gleam` with `State`, `Message`, `Outcome`, `WorkerDirectory`, `WorkerEntry`, `YamlStepEntry`, `PollState`, `RetryState`, `PendingClaim`, `PendingDispatchValidation`, `PendingLinearCommandAck`, `RecoveryStartup`, and any small supporting status types specified in Plan of Work.

5. Create `src/scherzo/orchestrator/transition.gleam` with `initial_state`, `handle`, and `snapshot` stubs. The initial `handle` should return the input state and a `Log("warn", "unsupported_transition_message", ...)` effect for messages that are not implemented yet.

6. Add `test/orchestrator_transition_test.gleam` with a minimal fixture that builds a `transition_types.State`, sends one unsupported message through `transition.handle`, and asserts that the returned state is unchanged and the warning log effect is returned.

7. Run formatting and tests:

       direnv exec . gleam format --check src test
       direnv exec . gleam test

   Commit this scaffold as a single logical change after both commands pass.

8. Add a failing interpreter test in a new `test/orchestrator_effect_interpreter_test.gleam` for ledger gating: given an `AppendLedger` effect with `ContinueWith(SpawnClaimedWorker(...))` and a fake ledger append success, `interpreter.apply` returns exactly one follow-up `LedgerAppendCompleted(..., Ok(...), ...)` and no direct worker start side effect.

9. Implement the minimal `src/scherzo/orchestrator/effects/interpreter.gleam` `ShellState`, `ApplyResult`, and ledger append adapter needed for the test in step 8. Keep all non-ledger effects as ignored warnings for now.

10. Add the failing companion test in `test/orchestrator_effect_interpreter_test.gleam`: fake ledger append failure for `ContinueWith(SpawnClaimedWorker(...))` returns `LedgerAppendCompleted(..., Error(...), ...)` and does not return or perform a worker start.

11. Implement the failure branch for ledger append continuations and make both ledger-gating tests pass.

12. Add a transition test in `test/orchestrator_transition_dispatch_test.gleam` that sends `LedgerAppendCompleted(..., SpawnClaimedWorker(...), Ok(...), ...)` for a pending claim fixture and expects a `StartWorker` effect. Run it to confirm it fails because dispatch handling does not exist yet.

13. Add the minimal `src/scherzo/orchestrator/transitions/dispatch.gleam` continuation handler and wire it through `transition.handle` so the test in step 12 passes. Do not move `dispatch_candidates` yet.

14. Add the failing append-failure test in `test/orchestrator_transition_dispatch_test.gleam`: `LedgerAppendCompleted(..., SpawnClaimedWorker(...), Error(...), ...)` logs the failure, removes or preserves the pending claim exactly as current daemon behavior requires, and emits no `StartWorker` effect. Implement the branch and record the chosen pending-claim behavior in Decision Log if it was not obvious from the current daemon code.

15. Add `daemon.Message` support for a shell-only ledger completion delivery if needed, or use the interpreter follow-up drain path if no public shell message is required. Update `daemon.gleam` only enough to compile; do not route normal daemon messages through the transition yet.

16. Run formatting and tests, then commit the ledger continuation protocol.

17. Add a failing worker synchronization test in `test/orchestrator_effect_interpreter_test.gleam`: interpreting a `StartWorker` effect with a fake successful worker spawn adds the same issue id, run id, session id, and command route id to the shell registry projection and returns `WorkerStartRegistered(...)`.

18. Implement `WorkerStart` interpretation using the existing worker spawn dependency and `worker_registry.gleam`. In the first pass, use fake dependencies in the test so no real worker process is started.

19. Add the failing worker-spawn failure test: fake worker spawn or monitor registration failure returns `WorkerStartFailed(...)`, does not leave a route subject in `ShellState.route_subjects`, and does not add a stale registry entry.

20. Implement the failure branch and make the worker synchronization tests pass.

21. Add route-id tests in `test/orchestrator_transition_worker_test.gleam`: allocating two worker route ids and one YAML route id produces deterministic ids and increments `next_route_sequence` without touching shell subjects.

22. Implement the pure `WorkerDirectory` route allocation helpers in `transition_types.gleam` or `transitions/worker.gleam` and make the route-id tests pass.

23. Run formatting and tests, then commit the worker synchronization protocol.

24. Add a polling red test in `test/orchestrator_transition_polling_test.gleam` for stale `PollTick`: a state with `poll.scheduled_generation` greater than the incoming generation returns unchanged state and a stale-tick log effect.

25. Extract only the stale-generation branch from `daemon.gleam` `handle_poll_tick` into `src/scherzo/orchestrator/transitions/polling.gleam`; leave `handle_poll_tick` as a wrapper or keep current behavior side by side until final cutover. Wire the pure branch through `transition.handle` and make the test pass.

26. Add the current-poll red test in `test/orchestrator_transition_polling_test.gleam`: a current `PollTick` updates poll in-flight state and emits the same first async effect currently produced by `handle_poll_tick`, either `Async(RefreshRunning(...))` or `Async(FetchCandidates(...))` depending on the fixture.

27. Move the current-poll acceptance and first-effect creation from `handle_poll_tick` into `transitions/polling.gleam`; keep timer scheduling interpreted by the old daemon path until Milestone 7.

28. Add red tests for `RunningRefreshFinished`, `CandidateFetchFinished`, and `LinearCommandFetchFinished` async results in `test/orchestrator_transition_polling_test.gleam`, one result kind per test. Each test asserts stale generation ignoring, error logging, phase completion, pending ack retry trigger, and next-poll scheduling for the relevant fixture.

29. Move the corresponding `SideEffectCompleted` branches from `daemon.gleam` into `transitions/polling.gleam` one branch at a time. After each branch, run `direnv exec . gleam test` before moving the next branch.

30. Add a temporary paired characterization test for polling if the old wrapper and new transition can both be invoked from a shared fixture. The test should compare the ordered effects or state deltas for current poll acceptance. Remove the paired test only after the daemon wrapper is deleted in Milestone 8.

31. Run formatting and tests, then commit the polling extraction.

32. Add a dispatch red test in `test/orchestrator_transition_dispatch_test.gleam` for a valid candidate: calling the new dispatch transition on a fixture emits `Async(ValidateDispatchClaim(...))` and records `PendingDispatchValidation` with the expected generation.

33. Extract the candidate-iteration and validation-start portion of `daemon.gleam` `dispatch_candidates` into `transitions/dispatch.gleam`; leave a daemon wrapper that delegates to the new function where possible.

34. Add red tests for dispatch validation results, one case per test: stale validation ignored, refreshed issue blocked by a new dependency records a blocked dependency report and does not claim, missing required fields logs `dispatch_validation_precondition_failed`, invalid workflow emits one invalid workflow report effect, and valid refreshed issue emits `Async(ClaimIssue(...))` while storing `PendingClaim`.

35. Move the dispatch validation completion branch from `daemon.gleam` into `transitions/dispatch.gleam` one case at a time. Run `direnv exec . gleam test` after the blocked-dependency case and after the valid-claim case.

36. Add red tests for handoff claim completion: claim success emits an `AppendLedger` with `ContinueWith(SpawnClaimedWorker(...))` and no `StartWorker`; claim failure logs, releases pending state, and continues with remaining candidates.

37. Move the handoff claim completion branch from `daemon.gleam` into `transitions/dispatch.gleam` and make the claim completion tests pass.

38. Add a paired characterization test for dispatch claim success/failure while the old wrapper exists. Feed the same fixture through the legacy wrapper and the transition facade and compare pending state changes and ordered effects. Keep the test until wrapper deletion.

39. Run formatting and tests, then commit the dispatch extraction.

40. Add retry red tests in `test/orchestrator_transition_retry_test.gleam` for stale retry ticks, active retry refresh start, duplicate refresh suppression, dependency-blocked cancellation, terminal issue claim release without rescheduling, no-slot backoff scheduling, and successful retry candidate dispatch.

41. Extract `daemon.gleam` `handle_retry_tick` into `src/scherzo/orchestrator/transitions/retry.gleam`, first only the stale and duplicate-suppression branches, then the active refresh branch.

42. Move the `RetryRefreshFinished` `SideEffectCompleted` branch into `transitions/retry.gleam` one tested case at a time. Keep calls to `core.handle_retry_candidate`, `core.schedule_retry_with_backoff`, `core.stop_retry_for_policy_invalid`, and `core.stop_retry_for_dependency_blocked` where they already model runtime state correctly.

43. Add a paired characterization test for retry refresh dependency-blocked cancellation while the old wrapper exists.

44. Run formatting and tests, then commit the retry extraction.

45. Add Linear command red tests in `test/orchestrator_transition_linear_commands_test.gleam` for command receipt ordering: `LinearCommandSeen` and `LinearCommandStarted` are in an `AppendLedger` with `ContinueWith(ApplyLinearCommand(...))`, and the command is not applied before that append success continuation.

46. Move `process_linear_command_comments` from `daemon.gleam` into `src/scherzo/orchestrator/transitions/linear_commands.gleam` with a daemon wrapper. Make the receipt-ordering test pass.

47. Add red tests for `apply_linear_transport_actions`: duplicate receipt suppression, startup outbox replay without duplicate command application, and completed-unacked command replay that emits only the ack path.

48. Move `apply_linear_transport_actions` into `transitions/linear_commands.gleam` and make those tests pass.

49. Add red tests for `apply_linear_submit_command`: successful command result emits `AppendLedger(..., ContinueWith(EnqueueLinearAck(...)))`; completion append failure emits no ack; completion append success emits an ack enqueue effect; ack failure leaves pending ack for later retry; ack success appends `OutboxCompleted` and `LinearCommandAcked` before pending ack removal is observable.

50. Move `apply_linear_submit_command` and `enqueue_linear_command_ack` into `transitions/linear_commands.gleam` in that order, using ledger continuations for all gated acks.

51. Add a paired characterization test for Linear command ack replay while old wrappers exist.

52. Run formatting and tests, then commit the Linear command extraction.

53. Add operator command red tests in `test/orchestrator_transition_operator_test.gleam`: pause sets `operator_paused`, resume clears it, retry rejects paused dispatch, retry rejects active/pending/claimed issues, park rejects claimed issues, unpark clears parked state, reload emits `Async(ReloadWorkflow(...))`, prompt/stop/abort for missing sessions return the current not-found or rejected result, and session-targeted command results use display names when a session is known.

54. Move `handle_operator_command` and `apply_operator_command_to_state` from `daemon.gleam` into `src/scherzo/orchestrator/transitions/operator.gleam`, continuing to call `control_command_handler.apply`.

55. Move `reload_workflow_for_operator`, `retry_issue_for_operator`, `retry_resolved_issue`, `park_issue_for_operator`, and `unpark_issue_for_operator` into `transitions/operator.gleam` one function at a time. Run `direnv exec . gleam test` after the retry functions and after the park/unpark functions.

56. Run formatting and tests, then commit the operator extraction.

57. Add worker lifecycle red tests in `test/orchestrator_transition_worker_test.gleam`. Cover worker updates, worker success, worker failure, worker down, stale worker results, stop/abort, YAML step started, YAML step update, YAML step command route readiness, YAML step finished, command route cleanup, and session lifecycle effects. This step repairs the missing worker test step from the original draft.

58. Move `handle_worker_finished` from `daemon.gleam` into `src/scherzo/orchestrator/transitions/worker.gleam` by first extracting the stale-result branch, then success classification, then failure classification, then cleanup and handoff effect decisions. Run tests after each branch group.

59. Move `handle_worker_down` into `transitions/worker.gleam` after the shell has converted `process.Down` into `transition_types.WorkerDownEvent`. Keep monitor decoding in `daemon.gleam` or the interpreter.

60. Move worker command route readiness and YAML step session decisions into `transitions/worker.gleam`. Use route ids only; do not pass process subjects into pure messages.

61. Add a paired characterization test for worker finished/down races while old wrappers exist. The test should prove a stale worker finish after down does not double-finish a run and does not publish duplicate retry lifecycle events.

62. Run formatting and tests, then commit the worker extraction.

63. Add recovery red tests in `test/orchestrator_transition_recovery_test.gleam`: recovered retry scheduling, cleanup enqueueing, outbox replay ack enqueueing without duplicate command processing, invalid outbox payload failure effects, recovery warnings as log effects, and recovered workflow resumption effects carrying recovery metadata.

64. Move `schedule_recovered_retry_timers`, `enqueue_recovered_cleanups`, and `enqueue_startup_recovery_outbox` from `daemon.gleam` into `src/scherzo/orchestrator/transitions/recovery.gleam`. Keep ledger replay, issue refresh, workflow finalization, and recovery/finalization ledger appends in startup shell code and document their startup-fatal behavior in comments near the startup sequence.

65. Run formatting and tests, then commit the recovery application extraction.

66. Add shutdown red tests in `test/orchestrator_transition_shutdown_test.gleam`: cancel poll and retry timers, stop all workers, clear pending claims and validations, request shutdown interruption projection load for active run ids, ignore empty interruption bodies, clear control file path state, request event hub stop, and route `EffectRunnerDown(process.Down)` through the shell-only fatal path rather than `transition.handle`.

67. Move pure portions of `shutdown_state`, `append_shutdown_step_attempt_interruptions`, and `shutdown_step_attempt_interruption_bodies` into `src/scherzo/orchestrator/transitions/shutdown.gleam`. Keep process stopping and effect-runner-down fatal cleanup in the shell or interpreter.

68. Move `shutdown_state_internal` decisions into `transitions/shutdown.gleam` or an interpreter shutdown helper according to whether each branch needs process-specific resources. Run tests after timer cancellation and after interruption ledger request creation.

69. Run formatting and tests, then commit the shutdown extraction.

70. Start the interpreter cutover by changing `daemon.State` to hold a `kernel: transition_types.State` and a `shell: effects_interpreter.ShellState` while leaving the old private fields in place if needed for wrappers. Run tests after this structural change before moving any behavior.

71. Move `append_ledger_bodies` from `daemon.gleam` into `effects/interpreter.gleam`. Run the ledger gating tests and full `direnv exec . gleam test`.

72. Move `enqueue_side_effect` and async effect-runner adaptation into `effects/interpreter.gleam`. Validate that `test/orchestrator_effect_runner_test.gleam` still passes and that daemon tests still receive async completion messages.

73. Move timer scheduling and cancellation from `apply_effect` into `effects/interpreter.gleam`. Validate poll and retry scheduler tests plus daemon retry timer tests.

74. Move worker process spawn, stop, abort, command send, route subject registration, and monitor registration into `effects/interpreter.gleam`. Validate worker registry tests, worker transition tests, and daemon worker process tests.

75. Move event publishing, control file removal, control server stop, event hub stop, cleanup execution, tracker/handoff/triage/Linear call adaptation, and shutdown effect handling into `effects/interpreter.gleam` one effect family at a time. Run `direnv exec . gleam test` after each family.

76. Update `daemon.gleam` so one shell message family at a time routes through `transition.handle` and `effects/interpreter.apply`: first polling, then retry, then dispatch async completions, then Linear command completions, then operator commands, then worker/YAML messages, then recovery and shutdown. After each family, drain `ApplyResult.follow_up_messages` in order and run the relevant transition tests plus full `direnv exec . gleam test`.

77. Keep `EffectRunnerDown(process.Down)` as a shell-only branch in `daemon.gleam`. It must call the interpreter shutdown helper and stop the actor abnormally; it must not become a `transition_types.Message`.

78. Update `get_snapshot` to route through `SnapshotRequested(reply_id, now_ms)` or return `transition.snapshot(state.kernel)` from the shell, whichever keeps public behavior simplest. Keep the public return type `orchestrator_state.RuntimeState`.

79. Update startup to build both `transition_types.State` and `effects/interpreter.ShellState`. Startup should still load the runtime bundle, construct clients, perform startup-fatal recovery loading and recovery/finalization ledger appends, start the event hub, start the control plane, start and monitor the effect runner, schedule the initial poll, and install the actor selector. The transition state should receive a data-only `RecoveryStartup` value; the interpreter state should receive shell resources.

80. Run formatting and tests. Commit the shell/interpreter cutover only after these commands pass:

       direnv exec . gleam format --check src test
       direnv exec . gleam test

81. Do not adapt once mode to the new transition/effect path in this ExecPlan. Run `test/orchestrator_service_test.gleam` through the full suite and fix only regressions caused by moved shared helpers. If an implementation change accidentally touches once mode, add a Decision Log entry and a service test that proves current behavior is preserved.

82. Remove legacy private wrappers from `daemon.gleam` in small groups matching the transition modules. After each group, run `direnv exec . gleam test`. If a removed wrapper was still needed for shell behavior, restore it and record why in Decision Log.

83. Review broad daemon tests. Move tests that now only assert pure decisions into the transition test files, and keep daemon tests that require actual actors, timers, process monitors, control server authentication, event hub behavior, or worker process routing. Do not delete a daemon integration test until an equivalent pure test exists or the test is intentionally shell-only.

84. Check that the named handler families are gone from `daemon.gleam` or are only thin wrappers with Decision Log justification. At minimum, search for these names:

       grep -n "fn handle_poll_tick\|fn dispatch_candidates\|fn handle_retry_tick\|fn process_linear_command_comments\|fn apply_linear_transport_actions\|fn apply_linear_submit_command\|fn enqueue_linear_command_ack\|fn handle_operator_command\|fn handle_worker_finished\|fn handle_worker_down\|fn shutdown_state_internal" src/scherzo/orchestrator/daemon.gleam

   Expect no matches, or matches that are short shell adapters documented in Decision Log.

85. Run final validation:

       jj status --color=never
       direnv exec . gleam format --check src test
       direnv exec . gleam test

   Expect the status to show only the intended implementation changes, formatting to pass, and all tests to pass. Update Outcomes & Retrospective with final module sizes, test coverage changes, intentionally retained daemon wrappers, and any open follow-up work.

## Testing and Falsifiability

This refactor is falsified if any existing daemon, service, control, Linear command, session event, recovery, or core test fails without an intentional and documented behavior change. It is also falsified if pure transition tests cannot assert effect order for ledger-sensitive flows, if interpreter tests show ledger continuations can start workers or enqueue acks before durable appends succeed, or if worker registry synchronization tests show the shell registry and pure `WorkerDirectory` can diverge after start, route registration, stop, finish, or down events. Passing broad integration tests alone is not sufficient; the plan's claim is maintainability through pure transitions, so the new tests must prove that meaningful behavior moved down from daemon integration tests into pure transition tests.

Add these new test files:

- `test/orchestrator_transition_test.gleam`: shared fixtures and smoke tests for `transition.handle` and effect assertions.
- `test/orchestrator_effect_interpreter_test.gleam`: ledger append policies, ledger continuation follow-up messages, claim append success and failure, Linear command ack gating, retry best-effort append behavior, worker start registration, worker spawn failure cleanup, route subject correlation, and shell registry synchronization.
- `test/orchestrator_transition_polling_test.gleam`: poll tick, stale generation, running refresh, candidate fetch, paused/no-slot behavior, Linear command phase completion, pending ack retry, and next-poll scheduling.
- `test/orchestrator_transition_dispatch_test.gleam`: candidate ordering, dispatch preconditions, blocked dependency reporting, invalid workflow reporting, final validation, pending claim creation, claim success/failure, claim ledger continuation, workflow route failures, and no-slot continuation.
- `test/orchestrator_transition_retry_test.gleam`: retry generation handling, refresh in-flight state, terminal/done/non-active issue handling, dependency-blocked cancellation, retry backoff, retry best-effort ledger append, and successful retry dispatch.
- `test/orchestrator_transition_linear_commands_test.gleam`: Linear command receipt ordering, command-start ledger gating, command completion ledger gating, ack outbox enqueue, ack retry, ack success, replay suppression, duplicate receipts, and ignored comments.
- `test/orchestrator_transition_operator_test.gleam`: pause/resume, retry/park/unpark, reload request, session-targeted route decisions, prompt and UI response size rejection, and command result formatting.
- `test/orchestrator_transition_worker_test.gleam`: worker updates, worker success, worker failure, worker down, worker start registration/failure messages, stop/abort, YAML step sessions, command route cleanup, route id allocation, and session lifecycle effects.
- `test/orchestrator_transition_recovery_test.gleam`: recovered retries, cleanup enqueueing, outbox replay, workflow resumption, recovery warning effects, and startup-fatal versus best-effort recovery boundary fixtures.
- `test/orchestrator_transition_shutdown_test.gleam`: timer cancellation, worker stop requests, control/event hub shutdown effects, pending state clearing, shutdown interruption ledger effects, and effect-runner-down shell-only behavior.

Add temporary paired characterization tests while old wrappers and new transitions coexist for these high-risk flows: dispatch claim success/failure, Linear command ack replay, retry refresh dependency-blocked cancellation, worker finished/down races, polling current-tick acceptance, and shutdown interruption ledger creation. Each paired test should feed the same fixture through the legacy wrapper and the transition facade, then compare state deltas and ordered effects. Remove a paired test only when the legacy wrapper it protects is deleted and an equivalent pure or interpreter test remains.

Add interpreter or daemon-shell tests for ledger-gated behavior. At minimum, cover:

- Claim append success returns `LedgerAppendCompleted(..., Ok(...), ...)`, the continuation transition emits `StartWorker`, and the interpreter then attempts worker spawn.
- Claim append failure returns `LedgerAppendCompleted(..., Error(...), ...)`, the continuation transition emits no `StartWorker`, and no worker registry entry or route subject remains.
- Linear command start append failure does not apply the operator command and does not enqueue an ack.
- Linear command completion append failure does not enqueue an ack.
- Linear ack success appends `OutboxCompleted` and `LinearCommandAcked` before pending ack removal is observable.
- Retry append failure under `ContinueRegardless` logs the configured failure event but keeps the scheduled retry semantics currently used by the daemon.

Move these categories of existing broad daemon tests down where possible:

- From `test/orchestrator_daemon_test.gleam`, move pure decisions behind `daemon_skips_invalid_workflow_candidate_and_reports_once_test`, `daemon_reports_invalid_workflow_candidate_when_slots_are_full_test`, `daemon_final_validation_blocks_new_dependency_test`, `daemon_final_validation_allows_terminal_blocker_test`, `daemon_retry_refresh_dependency_blocked_cancels_retry_test`, `daemon_retry_refresh_done_issue_releases_claim_without_rescheduling_test`, and `daemon_retry_timer_requeues_failed_worker_once_test` into dispatch or retry transition tests. Keep at least one daemon smoke test proving actor poll-to-worker wiring still works.
- From `test/orchestrator_daemon_control_test.gleam`, move pause/resume, retry reject/accept, park/unpark state mutation, and stale auto-park clearing into operator or dispatch transition tests. Keep control server authentication, control file write/remove, and actor reply delivery as daemon tests.
- From `test/orchestrator_daemon_linear_command_test.gleam`, move command ordering, ack retry, replay suppression, and duplicate receipt behavior into Linear command transition tests. Keep one daemon test proving comments fetched through the effect runner are delivered to the transition and ack effects are interpreted.
- From `test/orchestrator_daemon_session_event_test.gleam`, move session lifecycle ordering decisions into worker transition tests where possible. Keep event hub startup/failure, real publish-to-hub behavior, and process monitor race tests in daemon or interpreter tests.

Do not remove `test/orchestrator_core_test.gleam`; it remains the low-level domain safety net. Do not remove `test/orchestrator_effect_runner_test.gleam`; it remains the async concurrency safety net. Do not remove scheduler, registry, or service tests; they remain helper and no-regression safety nets. Once-mode sharing is not part of this plan, so service tests are used to prove no regression, not to require the new transition path.

Each new transition test should build a small state fixture, call one transition function or `transition.handle`, and assert both the returned state and the returned ordered effects. For ledger-sensitive tests, assert exact order, not merely membership. For example, a successful claim test should assert that the transition emits an `AppendLedger` effect containing `WorkflowRunStarted`, `KnownWorkspace`, `RunStarted`, and `IssueCounterUpdated` with `ContinueWith(SpawnClaimedWorker(...))`, and only the successful `LedgerAppendCompleted` continuation emits a `StartWorker` effect. A Linear ack success test should assert `OutboxCompleted` and `LinearCommandAcked` appear before pending ack removal is observable.

The final validation command is:

    direnv exec . gleam test

The expected result is that all tests pass. The exact pass count may change as tests are moved or added; record the final pass count in Outcomes & Retrospective.

## Validation and Acceptance

Acceptance is behavior-based. After implementation, a reviewer should be able to start from a clean repository, run:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

and observe a formatted tree with all tests passing.

The daemon API remains compatible. Code that calls `daemon.start`, `daemon.shutdown`, `daemon.get_snapshot`, or `daemon.apply_operator_command` should not need to change, except for internal test helpers that intentionally inspect private implementation details.

The actor shell is measurably smaller and narrower. `src/scherzo/orchestrator/daemon.gleam` should primarily contain public types, dependency defaults, startup and shutdown shell setup, shell message conversion, reply subject handling, selector setup, effect-runner-down shell handling, and calls into `transition.handle` and `effects/interpreter.apply`. It should not contain the large private functions mapped in Scope Boundaries unless a Decision Log entry explains why a specific function had to remain shell-local. The validation search in Concrete Step 84 should return no matches, or only short shell adapters documented in Decision Log.

The new transition modules exist and own the mapped behavior. A reviewer can inspect `src/scherzo/orchestrator/transitions/dispatch.gleam` to understand dispatch decisions without reading worker process spawn code, inspect `src/scherzo/orchestrator/transitions/retry.gleam` to understand retry decisions without reading control server code, inspect `src/scherzo/orchestrator/transitions/linear_commands.gleam` to understand receipt and ack behavior without reading timer code, and inspect `src/scherzo/orchestrator/transitions/worker.gleam` to understand worker lifecycle decisions without reading PID or monitor code.

Effects are explicit and typed. Ledger writes, timer scheduling, event publishing, worker spawning/stopping, async tracker/handoff/Linear calls, cleanup, control-plane operations, shutdown operations, and replies appear as `effects/types.gleam` values returned by pure transitions. Data-only effects do not carry concrete clients, process handles, process subjects, process timers, or dependency closures.

Ledger continuations are observable and safe. A worker is not spawned until the issue claim succeeds and the start records that currently gate spawning are appended successfully. A Linear ack is not enqueued until the command completion records that gate the ack are appended successfully. Retry best-effort ledger append failure keeps the current continue-and-log behavior. These invariants must be proven by interpreter or daemon-shell tests, not only by pure effect-order tests.

The worker mirror and shell registry stay synchronized. After a worker start succeeds, the same issue id, run id, session id, and route ids are visible in pure `WorkerDirectory` and the shell registry projection. After spawn failure, stop, finish, down, or YAML route cleanup, neither side retains stale route ids that would lose prompt, stop, abort, or YAML step commands. These invariants must be proven by transition and interpreter tests.

Startup recovery preserves current failure semantics. Ledger replay, issue refresh for recovery, workflow finalization, and recovery/finalization ledger appends remain startup-fatal. Retry timer scheduling, cleanup enqueueing, outbox ack replay, recovery warnings, and workflow resumption after a loaded recovery plan remain best-effort or logged according to current daemon behavior.

Once mode remains a no-regression surface only. `src/scherzo/orchestrator/service.gleam` should keep passing current once-mode tests. This ExecPlan does not require once mode to share the new transition/effect model. If full sharing is added anyway, this plan must first be updated with synchronous interpreter semantics, idle-loop termination, supported effect list, parity tests, and a Decision Log entry.

High-risk invariants must be preserved:

- Startup recovery appends recovery/finalization records before scheduling recovered retry timers, enqueueing cleanup, replaying outbox acks, or resuming workflow workers.
- A worker is not spawned until the issue claim succeeds and the start records that currently gate spawning are appended successfully.
- Pending claims, pending dispatch validations, and pending Linear command acks are removed exactly once and only in the same cases as before.
- Stale poll, retry, dispatch validation, and effect runner completion results are ignored based on generation or correlation id.
- Retry timers are scheduled and cancelled with the same generation semantics as before, including no-slot backoff and dependency-blocked cancellation.
- Worker down and worker finished races do not double-finish a run or publish stale retry lifecycle events to an exited session.
- Linear command receipt and outbox records are appended in the current order and replay does not reapply already completed commands.
- Event publishing order remains stable for worker updates before worker exit and for stop/abort lifecycle events.
- Shutdown still cancels timers, stops workers, appends interrupted step attempts when possible, stops the control plane, removes the control file, and stops the event hub.
- Effect runner process down still takes the shell-only abnormal shutdown path after cleanup.

## Rollout, Recovery, and Idempotence

This is an internal refactor with no data migration and no intended runtime behavior change. Rollout is the normal code review and test pipeline. There is no need for a runtime feature flag if each commit is green and behavior-preserving.

Work in small commits matching the milestones and the checkpoint steps. If a checkpoint fails validation, revert that checkpoint's commit rather than trying to debug multiple extracted domains at once. Because ledger format and public protocols are unchanged, rollback is a source rollback: return to the last passing commit. Do not partially deploy a commit that changes effect interpretation without its corresponding transition, interpreter, and daemon-shell tests.

During implementation, keep old daemon functions as wrappers until their replacement is covered by tests and wired through the transition facade. Deleting wrappers should be the last step for each domain. If an extraction gets stuck, leave the old wrapper in place, record the reason in Surprises & Discoveries and Decision Log, and continue only if the tree remains green.

Milestone 7 must not be a single high-blast-radius cutover. Introduce `ShellState` without changing behavior, then move one effect family into the interpreter, route one shell message family through `transition.handle`, validate, and repeat. The checkpoint order is ledger append, async effect enqueueing, timers, worker spawn/stop/route handling, event publishing, control operations, shutdown operations, polling messages, retry messages, dispatch completions, Linear command completions, operator commands, worker/YAML messages, recovery, and shutdown. Each checkpoint is reversible by restoring the previous daemon wrapper and rerunning the relevant tests.

Commands in this plan are idempotent. Running `direnv exec . gleam test` or `direnv exec . gleam format --check src test` multiple times is safe. Creating new modules is not idempotent if the files already exist; if an implementation is resumed, read the existing file and update the Progress section before editing. Timer scheduling, control server startup, worker process spawning, ledger writes, and Linear calls should only happen through tests or runtime code, not during plan implementation steps.

The interpreter must preserve cleanup safety. Empty workspace paths must still skip cleanup. Control file removal should tolerate a missing file as current behavior does. Event hub shutdown timeout should still log a warning rather than crashing shutdown. Effect runner crashes that are represented as typed async completion results should still flow through `AsyncCompleted`; effect runner process down should still bypass the pure kernel and shut down the daemon abnormally after cleanup.

## Artifacts and Notes

Inspected source-control status:

    jj status --color=never
    The working copy has no changes.

Measured relevant file sizes:

    wc -l src/scherzo/orchestrator/daemon.gleam src/scherzo/orchestrator/core.gleam src/scherzo/orchestrator/effect_runner.gleam src/scherzo/orchestrator/service.gleam
        5921 src/scherzo/orchestrator/daemon.gleam
        1118 src/scherzo/orchestrator/core.gleam
         479 src/scherzo/orchestrator/effect_runner.gleam
        1837 src/scherzo/orchestrator/service.gleam
        9355 total

Current orchestrator files found:

    src/scherzo/orchestrator/workflow_reloader.gleam
    src/scherzo/orchestrator/state.gleam
    src/scherzo/orchestrator/daemon.gleam
    src/scherzo/orchestrator/reason.gleam
    src/scherzo/orchestrator/event_publisher.gleam
    src/scherzo/orchestrator/service.gleam
    src/scherzo/orchestrator/worker_registry.gleam
    src/scherzo/orchestrator/poll_scheduler.gleam
    src/scherzo/orchestrator/control_command_handler.gleam
    src/scherzo/orchestrator/core.gleam
    src/scherzo/orchestrator/effect_runner.gleam
    src/scherzo/orchestrator/retry_scheduler.gleam
    src/scherzo/orchestrator/yaml_step_session.gleam

Representative current daemon functions to move or preserve are named in Scope Boundaries. The implementer should re-run searches before moving code because line numbers and helper names may drift after this plan is written.

## Interfaces and Dependencies

No new package dependencies are required. Keep using the dependencies already declared in `gleam.toml`.

The final transition public interface is:

    src/scherzo/orchestrator/transition.gleam

    pub fn initial_state(
      workflow_path: Option(String),
      bundle: runtime_bundle.RuntimeBundle,
      recovery: transition_types.RecoveryStartup,
    ) -> transition_types.State

    pub fn handle(
      message: transition_types.Message,
      state: transition_types.State,
    ) -> transition_types.Outcome

    pub fn snapshot(state: transition_types.State) -> orchestrator_state.RuntimeState

The final effect interpreter public interface is:

    src/scherzo/orchestrator/effects/interpreter.gleam

    pub type Clients {
      Clients(
        tracker_client: tracker.Client,
        handoff_client: handoff.Client,
        linear_command_client: linear.CommandClient,
        triage_client: linear_triage.TriageClient,
      )
    }

    pub type ShellState

    pub type ApplyResult {
      ApplyResult(
        shell: ShellState,
        follow_up_messages: List(transition_types.Message),
        outbound_replies: List(OutboundReply),
      )
    }

    pub type OutboundReply {
      OperatorCommandReply(reply_id: String, result: command.CommandResult)
      SnapshotReply(reply_id: String, snapshot: orchestrator_state.RuntimeState)
      ShutdownReply(reply_id: String)
    }

    pub fn new_shell_state(
      subject: process.Subject(daemon.Message),
      dependencies: daemon.RuntimeDependencies,
      clients: Clients,
      effect_runner: effect_runner.Handle,
      effect_runner_monitor: process.Monitor,
      event_hub: process.Subject(hub.Message),
      control_server: daemon.ControlServerHandle,
      control_file_path: Option(String),
    ) -> ShellState

    pub fn apply(
      shell: ShellState,
      transition_state: transition_types.State,
      effects: List(effects_types.Effect),
    ) -> ApplyResult

`Clients` is shell-owned and may contain concrete `tracker.Client`, `handoff.Client`, `linear.CommandClient`, and `linear_triage.TriageClient` values. It must never appear in `transition_types.State`, `transition_types.Message`, or any effect returned by a pure transition. `WorkerRouteSubject` is also shell-owned and may contain `process.Subject(command.WorkerCommand)` values because it never crosses into pure state. `OutboundReply` is data-only and carries only reply ids plus values; `ShellState` maps reply ids to real reply subjects.

The final `daemon.gleam` should retain these public functions and types unless a separate compatibility plan changes them:

    pub type StartupError
    pub type Message
    pub type TimerHandle
    pub type ControlServerHandle
    pub type RuntimeDependencies
    pub fn default_dependencies() -> RuntimeDependencies
    pub fn start(workflow_path: Option(String), dependencies: RuntimeDependencies) -> Result(actor.Started(process.Subject(Message)), StartupError)
    pub fn shutdown(subject: process.Subject(Message), timeout_ms: Int) -> Result(Nil, Nil)
    pub fn get_snapshot(subject: process.Subject(Message), timeout_ms: Int) -> Result(orchestrator_state.RuntimeState, Nil)
    pub fn apply_operator_command(subject: process.Subject(Message), operator_command: command.OperatorCommand, timeout_ms: Int) -> Result(command.CommandResult, Nil)

`daemon.Message` may keep shell-facing variants that contain process subjects or `process.Down`, because it is not the pure message type. It should add private correlation handling for reply ids and ledger or worker follow-up delivery as needed. `EffectRunnerDown(process.Down)` remains shell-owned and should not be mirrored in `transition_types.Message`.

`src/scherzo/orchestrator/core.gleam` remains a dependency of transition modules. Do not move its low-level helpers unless a later cleanup proves that a specific helper is only used by one transition module. If a helper is moved, keep a forwarding wrapper for one commit and remove it only after tests pass.

`src/scherzo/orchestrator/effect_runner.gleam` remains responsible for bounded concurrent async work. In the first implementation, adapt `effects_types.AsyncEffect` to the existing `effect_runner.Effect` variants in the interpreter. If a later cleanup changes `effect_runner.Effect` itself to be data-only, do it as a separate commit with `test/orchestrator_effect_runner_test.gleam` passing before and after.

`src/scherzo/orchestrator/worker_registry.gleam` remains the shell registry for real worker handles. The pure `WorkerDirectory` in `transition_types.gleam` is the transition mirror. The interpreter must update the registry and return `WorkerStartRegistered`, `WorkerStartFailed`, route-ready, stop, and cleanup follow-up messages so the kernel mirror can update itself. If keeping them synchronized becomes complex, add more invariant tests in `test/orchestrator_effect_interpreter_test.gleam` before continuing; do not move additional worker lifecycle code while the mirror and registry can diverge.

`PollState` and `RetryState` remain data-only. Existing `src/scherzo/orchestrator/poll_scheduler.gleam` and `src/scherzo/orchestrator/retry_scheduler.gleam` may still be used by the shell or interpreter to manage real timer handles, but pure transitions should only store logical generations, in-flight generations, due times, and retry reasons.

`SessionEffect`, `ControlEffect`, `ShutdownEffect`, `WorkerStart`, `WorkerStop`, `ReplyEffect`, `Clients`, `OutboundReply`, `PollState`, `RetryState`, and `WorkerDirectory` are intentionally specified in this plan because they set the architecture. Do not replace them with an open-ended effect map, untyped string commands, closures, process handles in pure state, or ad-hoc shell callbacks.

## Open Questions and Clarifications Needed

- [CLARIFY] Current once mode appears to ignore daemon-style Linear command comment polling even when Linear commands are enabled. This plan preserves that behavior and defers once-mode transition sharing. Confirm in a follow-up issue whether once mode should continue to skip Linear command polling, share daemon dispatch decisions through a synchronous interpreter, or intentionally gain Linear command polling behavior.
