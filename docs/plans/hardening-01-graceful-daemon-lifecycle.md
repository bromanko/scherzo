# Hardening 01: Add graceful daemon lifecycle and signal shutdown

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, stopping Scherzo daemon mode with SIGTERM is a graceful lifecycle event rather than an abrupt VM teardown. An operator can run `direnv exec . gleam run -- path/to/WORKFLOW.md`, send SIGTERM from a process manager, and observe Scherzo call the existing `daemon.shutdown` path, stop the local control server, remove the control file, stop daemon-owned workers, cancel poll and retry timers, release the local instance lock, and exit with a predictable status. The visible proof is a deterministic test that starts daemon mode with fake dependencies, sends a stop event through the new lifecycle seam, and asserts that `daemon_shutdown` is logged, `instance.lock` and `control.json` are gone, and a second daemon can start with the same workspace root without manual stale-lock cleanup. A production-path signal validation must also prove that SIGTERM reaches the lifecycle seam.

This phase does not make Scherzo recover after `kill -9`, host power loss, BEAM crash, terminal Ctrl-C/SIGINT, or distributed multi-host races. Local OTP validation rejects direct SIGINT trapping through `os:set_signal(sigint, handle)`, and the owner chose not to grow this phase into a wrapper or OTP-application packaging change. It makes ordinary process-manager shutdown safe and boring while documenting Ctrl-C/SIGINT as still abrupt in this phase.

## Problem Framing and Constraints

Current daemon mode starts the daemon actor and then calls `process.sleep_forever()` in `src/scherzo/orchestrator/service.gleam`. The instance lock is released only after that sleep returns, which it never does in normal CLI daemon mode. Programmatic `daemon.shutdown` is already testable and cancels timers, stops workers, stops the control server, and removes the control file, but the CLI path does not install a SIGTERM handler. README currently warns that shell or process-manager termination may leave a stale `workspace.root/.scherzo-state/instance.lock` file that operators must remove manually after verifying no process remains.

The operator problem is real: stale lock files and stale control files make normal restarts awkward and encourage unsafe manual cleanup. Process managers also expect a service to react to SIGTERM within a bounded timeout. Scherzo should use the daemon shutdown path it already has instead of relying on OS teardown for every normal stop.

The implementation must remain a Gleam Erlang-target application. It should preserve deterministic tests and avoid introducing a large supervision framework. The safest small change is to add a lifecycle waiter around the existing daemon actor and an Erlang FFI signal bridge for SIGTERM, which the runtime can trap with `os:set_signal(sigterm, handle)`. Do not add SIGINT support in this phase.

## Strategy Overview

Add a small lifecycle module, `src/scherzo/lifecycle.gleam`, and an Erlang FFI module, `src/scherzo_signal_ffi.erl`. The lifecycle module exposes a testable wait function that receives a daemon subject, an instance lock release function, timeout settings, and a stop event source. In production the stop event source comes from SIGTERM. In tests it is a normal Gleam subject, so deterministic tests never need to send real OS signals.

Change `service.start_daemon` so it no longer sleeps forever. It acquires the existing instance lock, creates the stop subject, installs the production SIGTERM stop source, starts the daemon, and blocks waiting for either SIGTERM or an explicit test stop. Installing the stop source before `daemon.start` closes the startup race where SIGTERM could arrive after the daemon creates `control.json` but before the handler exists. If signal installation fails, release the lock and return `StartupError("signal_handler_failed", message)` without starting the daemon. If daemon startup fails after signal installation, run the stop-source cleanup function and release the lock.

When a stop arrives, the lifecycle code logs `daemon_stop_requested`, calls `daemon.shutdown(subject, timeout_ms)`, logs `daemon_shutdown_complete` or `daemon_shutdown_timeout`, runs the stop-source cleanup function, releases the instance lock when the process is exiting, and returns. The daemon actor already owns control-server cleanup and worker shutdown through `Shutdown`. This plan should not duplicate that cleanup in service code. Service code only requests shutdown, waits for the ack, cleans up signal registration, and releases the outer instance lock.

Add production signal handling with an Erlang FFI wrapper around Erlang/OTP signal handling for SIGTERM only. The SIGTERM path must use the registered `erl_signal_server` gen_event manager, not an `erl_signal_server` module. Local validation found that the default `erl_signal_handler` also receives SIGTERM and starts VM shutdown immediately, so the FFI must temporarily replace that default handler while daemon mode is running. Use `os:set_signal(sigterm, handle)` and `gen_event:swap_handler/3` (or an equally atomic gen_event replacement) to install a custom `{scherzo_signal_ffi, Ref}` handler for `sigterm`. That handler forwards only the first `sigterm` event as the Gleam `Sigterm` stop reason to the provided stop subject, ignores duplicate SIGTERM events, and leaves `prim_tty_sighandler` untouched. Cleanup must be idempotent: remove the custom handler if present and restore `erl_signal_handler` only if this install removed it and no `erl_signal_handler` is currently registered. The FFI must not claim SIGINT support in this phase. The public Gleam API should hide Erlang message details, should report that SIGTERM was installed, and should expose the BEAM OS pid in metadata so production validation can target the real VM process.

## Alternatives Considered

One alternative is to keep relying on process managers and OS teardown. That is the current behavior and leaves stale instance locks, stale control files, and unclear worker cleanup on ordinary restarts.

Another alternative is to move Scherzo into a full OTP application/supervision tree immediately. That may be worthwhile later if Ctrl-C needs graceful handling, but it is a larger lifecycle shift than needed to make process-manager SIGTERM call `daemon.shutdown`. The owner explicitly rejected growing this phase for wrapper or OTP-application packaging work.

A third alternative is to add a separate `scherzoctl shutdown` command and tell operators to use it before terminal interrupts. That would be useful eventually, but it does not solve process-manager SIGTERM and does not make accidental Ctrl-C graceful unless SIGINT is separately handled.

A fourth alternative is to release the instance lock in an `after`-style helper without shutting down the daemon actor. That would make restarts easier but unsafe: another daemon could start while the old daemon is still running. The lock should be released only as part of process exit after shutdown has been requested.

## Risks and Countermeasures

The main lifecycle risk is double shutdown when multiple supported stop events arrive, such as repeated SIGTERM. Countermeasure: the lifecycle module records that shutdown has started and ignores subsequent stop messages except for logging `daemon_stop_already_in_progress` if useful.

The main safety risk is releasing the instance lock while an old daemon is still capable of dispatching work. Countermeasure: the normal path releases the lock after `daemon.shutdown` acknowledges. If shutdown times out, the service is exiting anyway; log the timeout clearly and still exit through one code path. Do not release the lock early before requesting shutdown.

The main worker risk is waiting forever for a worker that ignores graceful abort. Countermeasure: keep using existing `daemon.shutdown`, which calls the daemon's `stop_worker` helper. The daemon already escalates to killing worker processes when necessary. This plan adds a bounded shutdown timeout at the service lifecycle layer and documents timeout behavior.

The main FFI risk is relying on Erlang signal internals that are not exposed through a stable Gleam API. Countermeasure: the signal capability spike and follow-up probe are recorded before implementation. SIGTERM is the production signal path because local OTP evidence shows `os:set_signal(sigterm, handle)` succeeds; SIGINT is out of scope because direct `os:set_signal(sigint, handle)` fails and the owner rejected wrapper or OTP-application packaging work for this phase. The FFI must use `gen_event` against the registered `erl_signal_server`, must replace all registered `erl_signal_handler` handlers during daemon mode so the VM does not shut down before `daemon.shutdown`, and must restore one default handler during cleanup. Isolate all Erlang signal details in `src/scherzo_signal_ffi.erl`, add a production-independent test path for lifecycle behavior, and require a production-path SIGTERM validation that starts the CLI process, sends SIGTERM to the BEAM OS pid, and observes cleanup. If the exact Erlang signal bridge needs adjustment, only the FFI module changes.

The main regression risk is breaking existing `--once`, `--linear-smoke`, `--pi-probe`, and `ctl` modes. Countermeasure: only daemon mode changes. Existing mode parser tests must continue passing, and tests should prove `--linear-smoke` still does not acquire the lock.

The main startup race risk is a SIGTERM arriving after the daemon has started but before handlers are installed. Countermeasure: create the stop subject and install the production stop source before `daemon.start`; if any later startup step fails, run the signal cleanup function and release the lock.

## Progress

- [x] (2026-04-29 04:20Z) Drafted this plan after current validation: `direnv exec . gleam test` passed with `200 passed, no failures`.
- [x] (2026-04-29 04:30Z) Reviewed the plan and added the required signal capability spike after verifying local OTP accepts `sigterm` but rejects `sigint` through `os:set_signal/2`.
- [x] (2026-04-29 04:45Z) Resolved owner clarification: ship graceful SIGTERM only in this phase, do not add wrapper/OTP-application SIGINT work, and return nonzero after shutdown timeout while releasing the local lock during CLI exit.
- [x] (2026-04-29 04:45Z) Ran and recorded the signal capability spike in the implementation environment before writing the signal FFI.
- [x] (2026-04-29 15:06Z) Reviewed this plan adversarially and tightened the signal-handler replacement, subject-ownership, validation, and commit-point instructions before implementation.
- [ ] Add lifecycle stop-source abstractions and deterministic lifecycle tests.
- [ ] Add Erlang SIGTERM FFI and production signal registration.
- [ ] Change `service.start_daemon` to wait on lifecycle stop instead of `process.sleep_forever()`.
- [ ] Add integration tests for lock/control-file cleanup on graceful stop.
- [ ] Update README and help text to describe graceful SIGTERM support and continued abrupt Ctrl-C/SIGINT behavior in this phase.

## Surprises & Discoveries

- Observation: The current daemon actor already has a programmatic shutdown path.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` exports `shutdown(subject, timeout_ms)` and handles `Shutdown(reply)` by calling `shutdown_state`, logging `daemon_shutdown`, replying, and stopping the actor.

- Observation: The current CLI daemon path still blocks forever after daemon startup.
  Evidence: `src/scherzo/orchestrator/service.gleam` calls `process.sleep_forever()` inside `start_daemon` after `daemon.start` succeeds.

- Observation: README explicitly documents stale lock recovery after shell/process-manager termination.
  Evidence: `README.md` says CLI mode does not install SIGINT/SIGTERM handlers and stale `instance.lock` files may need manual removal.

- Observation: Erlang/OTP in the local development environment supports `os:set_signal(sigterm, handle)` but rejects `os:set_signal(sigint, handle)`.
  Evidence: `direnv exec . erl -noshell -eval 'io:format("sigint: ~p~n",[catch os:set_signal(sigint, handle)]), io:format("sigterm: ~p~n",[catch os:set_signal(sigterm, handle)]), halt().'` printed a `badarg` exit for `sigint` and `ok` for `sigterm`.

- Observation: `erl_signal_server` is a registered `gen_event` manager, not a callable Erlang module.
  Evidence: `direnv exec . erl -noshell -eval 'io:format("~p~n", [catch erl_signal_server:module_info(exports)]), halt().'` returned `undef`, while `gen_event:which_handlers(erl_signal_server)` returned handlers including `erl_signal_handler` and `prim_tty_sighandler`.

- Observation: Leaving the default `erl_signal_handler` installed makes SIGTERM shut down the VM before a custom handler can finish a grace period.
  Evidence: a local probe installed a custom handler, sent SIGTERM, slept for three seconds in the receiving process, and the VM exited after about one second with `SIGTERM received - shutting down`; the probe never wrote its post-sleep marker.

- Observation: Removing or replacing the default `erl_signal_handler` lets a custom SIGTERM handler complete cleanup, and the default handler can be restored afterward.
  Evidence: a local probe used `gen_event:delete_handler(erl_signal_server, erl_signal_handler, remove)`, received `sigterm`, slept for three seconds, wrote its post-sleep marker, and exited normally; `gen_event:add_handler(erl_signal_server, erl_signal_handler, [])` also returned `ok` in a separate restoration check.

- Observation: `gen_event:add_handler(erl_signal_server, erl_signal_handler, [])` can register duplicate default handlers if called when `erl_signal_handler` is already present.
  Evidence: a local restoration check called `gen_event:add_handler` while `erl_signal_handler` was already registered and `gen_event:which_handlers(erl_signal_server)` then showed two `erl_signal_handler` entries. Cleanup must check `which_handlers` before restoring the default handler.

## Decision Log

- Decision: Keep the existing daemon actor shutdown API as the single cleanup path.
  Rationale: It already owns timers, workers, control server, and control-file cleanup. Duplicating cleanup in service code would create two inconsistent lifecycle implementations.
  Date: 2026-04-29

- Decision: Add a small SIGTERM signal bridge instead of converting Scherzo to a full OTP application in this phase.
  Rationale: The immediate problem is ordinary process-manager shutdown. A SIGTERM signal bridge is smaller, testable, and does not force packaging changes. Direct SIGINT trapping failed in the local OTP check, and the owner chose not to grow this phase to include a wrapper or OTP-application packaging change.
  Date: 2026-04-29

- Decision: Replace the default `erl_signal_handler` while daemon mode is running and restore it during signal cleanup.
  Rationale: Local validation showed that adding a custom handler without removing the default handler still lets the default OTP handler stop the VM before graceful daemon cleanup can complete. Replacing the default handler confines the blast radius to daemon mode and cleanup restores normal VM behavior for startup failures and tests.
  Date: 2026-04-29

- Decision: Test lifecycle behavior through injected stop subjects, and require production-path SIGTERM validation before accepting signal support.
  Rationale: Unit tests should be deterministic and not depend on OS signal timing, but fake stop subjects cannot prove that the Erlang signal bridge works. A separate integration/manual validation must prove the production FFI path for SIGTERM.
  Date: 2026-04-29

- Decision: Install the production stop source before starting the daemon actor.
  Rationale: This closes the race where SIGTERM could arrive after `daemon.start` creates `control.json` but before a handler is registered. If startup later fails, the service can run the stop-source cleanup function and release the instance lock.
  Date: 2026-04-29

- Decision: Any test that runs `lifecycle.run_until_stop` in a spawned process must create the stop subject inside that spawned process and send the subject back to the test process before the test sends stop messages.
  Rationale: Gleam `process.Subject` values are owned by the process that creates them; receiving from a subject owned by another process panics. The service path creates and receives the stop subject in the service process, but spawned tests need an explicit ready-subject handshake.
  Date: 2026-04-29

- Decision: On daemon shutdown timeout, log `daemon_shutdown_timeout`, run stop-source cleanup, release the local instance lock as part of CLI exit, return `StartupError("daemon_shutdown_timeout", "daemon shutdown timed out")`, and let `src/scherzo/main.gleam` halt nonzero.
  Rationale: The owner accepted this policy. It gives process managers a failure signal while keeping the local workspace from requiring stale-lock cleanup when the CLI process is about to terminate.
  Date: 2026-04-29

## Outcomes & Retrospective

(To be filled at completion. Record the exact signal FFI behavior, shutdown timeout chosen, final test count, and the result of the required real SIGTERM validation.)

## Context and Orientation

Scherzo is a Gleam Erlang-target daemon. The CLI entry point is `src/scherzo/main.gleam`; it parses daemon, once, smoke, probe, and control modes. Production daemon mode calls `service.start_daemon` in `src/scherzo/orchestrator/service.gleam`.

The daemon actor lives in `src/scherzo/orchestrator/daemon.gleam`. It starts the EventHub, local control server, control file, poll timer, retry timers, and worker processes. It handles `Shutdown(reply)` by stopping control server, removing the control file, canceling timers, stopping workers, clearing runtime worker/control state, logging `daemon_shutdown`, replying to the caller, and stopping the actor.

The local instance lock lives in `src/scherzo/instance_lock.gleam`. It creates `workspace.root/.scherzo-state/instance.lock` with exclusive creation and removes it only when `instance_lock.release(lock)` is called.

At plan-authoring time, `service.start_daemon` acquires the lock and starts the daemon but then sleeps forever, so the release call after sleep is unreachable during ordinary CLI operation.

## Preconditions and Verified Facts

Before implementing this plan:

- `direnv exec . gleam test` passes. On 2026-04-29 the suite reported `200 passed, no failures`.
- `src/scherzo/orchestrator/daemon.gleam` exports `shutdown(subject, timeout_ms)`.
- `src/scherzo/orchestrator/service.gleam` owns instance-lock acquisition for daemon mode and currently calls `process.sleep_forever()` after successful `daemon.start`.
- `src/scherzo/main.gleam` exits with status 1 when `service.start_daemon` returns `Error(StartupError(...))`, so lifecycle timeout and signal install failures can surface as nonzero CLI exits by returning `Error`.
- The local control API writes a control file and daemon shutdown removes it. `test/orchestrator_daemon_control_test.gleam` already has `daemon_shutdown_closes_control_server_and_removes_control_file_test`, which can guide service-level cleanup assertions.
- Local OTP evidence shows `os:set_signal(sigterm, handle)` returns `ok` and `os:set_signal(sigint, handle)` raises `badarg`; this phase implements SIGTERM only and must not claim Ctrl-C/SIGINT support.
- Local OTP evidence shows `erl_signal_server` is a registered `gen_event` manager with default handlers `erl_signal_handler` and `prim_tty_sighandler`. There is no callable `erl_signal_server` module API. A custom handler must use `gen_event` and must remove every registered `erl_signal_handler` during daemon mode, otherwise a remaining default handler will stop the VM before graceful shutdown completes.
- Gleam `process.Subject` values are receive-owned by the process that creates them. Production `service.start_daemon` may create the stop subject and then receive it in the same service process. Tests that spawn a lifecycle waiter must create the stop subject inside the spawned waiter and send that subject back to the parent before the parent sends stop messages.
- No durable scheduler state exists yet. This plan must not pretend to recover after process death; it only handles graceful SIGTERM while the BEAM VM is alive.

## Scope Boundaries

In scope: SIGTERM handling for daemon mode; lifecycle wait abstraction; deterministic tests for graceful stop; lock release on graceful stop; control-file removal through existing daemon shutdown; worker stop through existing daemon shutdown; timeout logging; README/help updates that describe graceful SIGTERM and continued abrupt Ctrl-C/SIGINT behavior.

Out of scope: SIGINT/Ctrl-C graceful handling; wrapper or OTP-application packaging work for terminal interrupts; pretending unsupported SIGINT handling works; `kill -9` recovery; BEAM crash recovery; durable scheduler ledger; durable Linear command receipts; event archive; distributed claiming; changing `--once`, `--linear-smoke`, `--pi-probe`, or `ctl` behavior except for shared helper refactors.

## Milestones

Milestone 0 was the signal capability spike and owner scope decision. It recorded that Erlang/OTP can trap SIGTERM through `os:set_signal(sigterm, handle)` but rejects direct `os:set_signal(sigint, handle)`, and it resolved this phase to ship graceful SIGTERM only without wrapper or OTP-application packaging work.

Milestone 1 adds a testable lifecycle waiter. At the end, tests can simulate a stop event with a Gleam subject and prove that the waiter calls a fake shutdown function once, releases a fake lock once, runs stop-source cleanup once, and ignores duplicate stop events.

Milestone 2 adds the production SIGTERM signal bridge. At the end, daemon mode can install a custom SIGTERM handler through `gen_event`, temporarily replace the default `erl_signal_handler`, forward the first `sigterm` event as a `Sigterm` stop reason into the lifecycle waiter, ignore duplicate SIGTERM events, and restore the default handler during cleanup.

Milestone 3 wires daemon mode through the lifecycle waiter. At the end, `service.start_daemon` no longer calls `process.sleep_forever()` and SIGTERM stop events call `daemon.shutdown` with a timeout before releasing the instance lock. The handler is installed before `daemon.start`, and all startup-failure branches release the lock and clean up signal registration.

Milestone 4 validates cleanup and documentation. At the end, tests prove graceful stop removes the control file and releases the lock, a production-path SIGTERM validation has been recorded against the BEAM OS pid reported by the signal installation metadata, and README/help text accurately describes graceful SIGTERM support and continued abrupt Ctrl-C/SIGINT behavior.

## Plan of Work

The signal capability spike is complete and resolved: implement graceful SIGTERM only. Do not proceed with code that claims SIGINT/Ctrl-C support, and do not add wrapper or OTP-application packaging work in this phase.

Create `src/scherzo/lifecycle.gleam`. Define `StopReason` variants for `Sigterm` and `TestStop(String)`. Define `ShutdownResult` variants for `ShutdownComplete` and `ShutdownTimedOut`. Define `run_until_stop` so tests can inject a stop subject, fake shutdown function, fake release function, fake stop-source cleanup function, and logger. The function waits for the first stop reason, logs `daemon_stop_requested`, calls shutdown exactly once, logs completion or timeout, runs cleanup exactly once, releases exactly once, and returns the shutdown result. Later stop messages must not trigger a second shutdown.

In tests, do not use real OS signals. Because a `process.Subject` can only be received by its owner process, spawned lifecycle tests must use a ready-subject handshake: the spawned process creates the stop subject, sends that stop subject back to the parent test process, and then calls `run_until_stop` with the subject it owns. The parent sends `TestStop("test")` to that returned subject and asserts fake shutdown, stop-source cleanup, and release subjects each receive exactly one message. For duplicate-stop tests, make the fake shutdown function notify the test that shutdown has started and then block on a test-controlled continue subject; while shutdown is blocked, send a second stop message, release the fake shutdown, and prove only the first message starts shutdown.

Create `src/scherzo_signal_ffi.erl` and a small `src/scherzo/signal.gleam` wrapper. The Erlang module should export `install_sigterm/1`, `cleanup_sigterm/1`, and the `gen_event` callbacks `init/1`, `handle_event/2`, `handle_call/2`, `handle_info/2`, `terminate/2`, and `code_change/3`. `install_sigterm(subject)` should call `os:set_signal(sigterm, handle)`, build a unique handler id such as `{scherzo_signal_ffi, make_ref()}`, and use `gen_event:swap_handler(erl_signal_server, {erl_signal_handler, scherzo_takeover}, {HandlerId, Subject})` when `erl_signal_handler` is present. If the default handler is not present, install the custom handler with `gen_event:add_handler/3` and record that no default handler needs restoration. After the custom handler is installed, call `gen_event:which_handlers(erl_signal_server)` and ensure no `erl_signal_handler` entries remain; if duplicates exist because of prior cleanup bugs or repeated tests, remove the remaining default handlers or fail installation after cleaning up the custom handler. The custom handler must normalize the `swap_handler` init argument shape, keep state with the target Gleam subject and a delivered flag, and on the first `sigterm` event call `gleam@erlang@process:send(Subject, sigterm)`. The atom `sigterm` is the Erlang representation of the no-argument Gleam constructor `lifecycle.Sigterm`; keep the constructor name stable and cover this path with production validation. Ignore duplicate `sigterm` events and any unrelated signal events. `cleanup_sigterm(handle)` must be safe to call more than once: delete the custom handler if it is still installed and restore one `erl_signal_handler` with `gen_event:add_handler(erl_signal_server, erl_signal_handler, [])` only if this install replaced a default handler and `gen_event:which_handlers(erl_signal_server)` does not already include `erl_signal_handler`. Keep `prim_tty_sighandler` untouched. Keep all Erlang signal details inside these two modules, and make install fail clearly if SIGTERM cannot be registered or the handler cannot be swapped.

Modify `src/scherzo/orchestrator/service.gleam`. Add a public or test-visible helper with a concrete shape like `start_daemon_with_lifecycle(workflow_path, lifecycle_dependencies)`, where `lifecycle_dependencies` contains a `daemon.RuntimeDependencies` value, `install_stop_source: fn(process.Subject(lifecycle.StopReason)) -> Result(signal.Installation, String)`, `shutdown_timeout_ms: Int`, and `lifecycle_logger: fn(String, String, List(log.Field)) -> Nil`. Production `start_daemon` uses `daemon_dependencies()`, `signal.install`, a 10 second timeout, and a wrapper around `log_stderr(level, event, fields, [])`. Create the stop subject and call `lifecycle.run_until_stop` in the same service process so subject ownership remains valid. After successful signal installation, log `signal_handler_installed` with fields `signal=sigterm` and `os_pid=<installation.os_pid>` before starting the daemon, so the production validation can target the BEAM VM rather than a `gleam` or `direnv` wrapper process.

The service helper normal path should:

1. acquire the instance lock,
2. create the lifecycle stop subject,
3. install the production stop source, keep its cleanup function, and log `signal_handler_installed signal=sigterm os_pid=<installation.os_pid>`,
4. start the daemon actor,
5. wait for a stop event through `lifecycle.run_until_stop`, using a shutdown callback that calls `daemon.shutdown(started.data, shutdown_timeout_ms)`,
6. let the lifecycle waiter run stop-source cleanup and release the instance lock exactly once,
7. return `Ok(Nil)` for graceful shutdown or `Error(StartupError("daemon_shutdown_timeout", "daemon shutdown timed out"))` for timeout.

If signal installation fails, release the lock and return `Error(StartupError("signal_handler_failed", message))` without starting the daemon. If `daemon.start` fails after signal installation, run the stop-source cleanup function and release the lock exactly as it releases the lock today. If `daemon.shutdown` times out, log `daemon_shutdown_timeout`, run signal cleanup, release the lock as part of CLI exit, and return the timeout error so `src/scherzo/main.gleam` exits nonzero.

Add a daemon lifecycle integration test. Use a temporary workflow, fake daemon dependencies based on the existing `daemon.RuntimeDependencies` test patterns, and a fake stop-source installer. The fake installer receives the service-owned stop subject, sends that subject to the parent test through a ready subject, and returns an installation whose cleanup function sends a cleanup notification. Run the service helper in a spawned process because it blocks waiting for a stop event, wait for the ready subject, send the injected stop event to the returned stop subject, and assert shutdown logged, signal cleanup called, control file removed, lock released, and a second lock acquire succeeds.

Update `src/scherzo/main.gleam` usage text and README to remove the old blanket warning that both SIGINT and SIGTERM are unsupported. Explicitly say SIGTERM is graceful and Ctrl-C/SIGINT remains unsupported or abrupt in this runtime. Keep the warning that `kill -9` or VM crash can still leave stale locks.

## Concrete Steps

Commit points for this phase are: after Step 6 when lifecycle unit tests pass, commit the lifecycle waiter; after Step 15 when service lifecycle tests pass, commit service wiring and signal abstractions; after Step 19 when documentation and production-path validation are complete, commit the final docs and validation record. At each commit point, run at least `direnv exec . gleam format --check src test` and `direnv exec . gleam test`, and do not commit a broken tree.

1. From the repository root, run `direnv exec . gleam test` and record the current pass count in Progress. Expected baseline at plan review time is `200 passed, no failures`.

2. Confirm the signal spike result already recorded in Surprises & Discoveries: `direnv exec . erl -noshell -eval 'io:format("sigint: ~p~n",[catch os:set_signal(sigint, handle)]), io:format("sigterm: ~p~n",[catch os:set_signal(sigterm, handle)]), halt().'` reported `sigterm: ok` and a `badarg` exit for `sigint`. Do not add SIGINT/Ctrl-C handling in this phase.

3. Create `test/lifecycle_test.gleam`. Add `run_until_stop_calls_shutdown_cleanup_and_release_once_test`: create parent-owned subjects for ready, shutdown-called, signal-cleanup-called, release-called, and result; spawn a process that creates the stop subject, sends that stop subject to ready, and then calls `lifecycle.run_until_stop`; after the parent receives the stop subject, send `TestStop("test")`; assert shutdown, cleanup, and release messages are each received once and the result is `ShutdownComplete`.

4. In `test/lifecycle_test.gleam`, add `run_until_stop_ignores_duplicate_stop_messages_test`: use the same ready-subject pattern, make the fake shutdown send a `shutdown_started` message and then block on a parent-owned continue subject, send a second `TestStop("duplicate")` while shutdown is blocked, release the fake shutdown, and assert only one shutdown call, one cleanup call, and one release call happen.

5. In `test/lifecycle_test.gleam`, add `shutdown_timeout_returns_error_test`: use a fake shutdown function that returns `Error(Nil)` to simulate timeout; assert the lifecycle returns `ShutdownTimedOut`, logs `daemon_shutdown_timeout`, and still runs stop-source cleanup and release once.

6. Implement `src/scherzo/lifecycle.gleam` until the lifecycle tests pass. Keep this module free of OS signal code. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`; if both pass, commit this unit as `Add daemon lifecycle waiter`.

7. Add `src/scherzo/signal.gleam` and `src/scherzo_signal_ffi.erl`. Expose `install(subject: process.Subject(lifecycle.StopReason)) -> Result(signal.Installation, String)` or equivalent, where `Installation` contains a cleanup function, `installed_signals: List(lifecycle.StopReason)` containing `Sigterm`, and `os_pid: String`. The cleanup function must call the FFI cleanup handle, remove the custom gen_event handler if present, and restore `erl_signal_handler` only if this install replaced it and no default handler is currently registered.

8. Add tests for `signal.gleam` that do not send real OS signals: at minimum, expose a test-visible `install_with_ffi(subject, ffi_install, ffi_cleanup)` or equivalent helper, use fake FFI functions to assert the wrapper returns `Ok(installation)` when install succeeds, maps FFI failure to `Error(message)`, reports `installed_signals == [Sigterm]`, preserves `os_pid`, and calls cleanup once even if the installation cleanup function is invoked twice. Production signal delivery and the Erlang `sigterm` atom mapping are validated later by a manual or integration run.

9. In `src/scherzo/orchestrator/service.gleam`, add the `DaemonLifecycleDependencies` helper type described in Plan of Work and implement `start_daemon_with_lifecycle(workflow_path, dependencies)`. The helper must create the stop subject in the service process, install the stop source before `daemon.start`, log `signal_handler_installed signal=sigterm os_pid=<installation.os_pid>` after successful installation, call `lifecycle.run_until_stop` from the same service process, and release the lock on every startup failure branch.

10. Add service tests for failure cleanup: one test where `install_stop_source` returns `Error("boom")` and a second lock acquire succeeds afterward; one test where `daemon.start` fails after successful stop-source installation and the cleanup subject receives one message before the lock is released. Force daemon startup failure with `daemon.RuntimeDependencies` such as a `start_event_hub` function returning `Error`, rather than adding a second daemon-start abstraction just for tests.

11. Update production `start_daemon` to call the helper with real SIGTERM signal installation and a shutdown timeout such as 10 seconds.

12. Add `test/orchestrator_service_lifecycle_test.gleam`. Spawn `service.start_daemon_with_lifecycle` because it blocks. In the fake stop-source installer, send the received service-owned stop subject to the parent test through a ready subject, then return an installation with cleanup metadata. After the parent receives the stop subject, send `TestStop("service-test")` and assert the spawned service result is `Ok(Nil)`.

13. Extend the service lifecycle test to assert the instance lock is removed: acquire should fail while the daemon is running and succeed after graceful stop.

14. Extend or add a control-file lifecycle test: after daemon start, read the control file path from the daemon logger or `control_file.path_for_workspace(root)`; after graceful stop, assert the file no longer exists.

15. Add a timeout service test: make the lifecycle shutdown callback return timeout, assert `start_daemon_with_lifecycle` returns `Error(StartupError("daemon_shutdown_timeout", _))`, cleanup is called once, and the lock is released according to the accepted timeout policy in Decision Log. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`; if both pass, commit this unit as `Wire daemon lifecycle shutdown`.

16. Update `src/scherzo/main.gleam` usage text to say daemon mode handles SIGTERM gracefully and Ctrl-C/SIGINT may still terminate abruptly. Update `test/main_test.gleam` so it no longer expects the old blanket `SIGINT/SIGTERM` unsupported phrase and instead asserts both graceful `SIGTERM` and abrupt `Ctrl-C` or `SIGINT` wording.

17. Update README sections `Daemon behavior and shutdown`, `Local control API and scherzoctl`, and `Implemented coverage and current limits` to reflect graceful SIGTERM support, continued abrupt Ctrl-C/SIGINT behavior, and the `kill -9`/VM crash stale-lock warning.

18. Run `direnv exec . gleam format`, `direnv exec . gleam format --check src test`, and `direnv exec . gleam test`. Record the final pass count in Progress.

19. Required production-path validation: run daemon mode with a temporary workflow, send SIGTERM to the BEAM OS pid reported in the `signal_handler_installed` log line, and verify the process exits with status 0 within the 10 second shutdown timeout, the log contains `daemon_stop_requested reason=sigterm`, `daemon_shutdown`, and `daemon_shutdown_complete`, `instance.lock` is gone, `control.json` is gone, and the next daemon start can acquire the same workspace lock. Use a workflow with `agent.max_concurrent_agents: 0`, `LINEAR_API_KEY=test-key`, a long polling interval, and a harmless fake pi command so validation does not launch real agents. Record the exact command, log excerpt, PID targeting method, exit status, and cleanup result in Outcomes. If the process does not exit promptly after `daemon_shutdown_complete`, do not accept the phase; add the smallest fix that makes daemon-mode success exit explicit, such as halting with status 0 from `src/scherzo/main.gleam` after graceful daemon shutdown.

20. Commit the final documentation and validation record with a message such as `Handle graceful daemon shutdown signals`.

## Testing and Falsifiability

This plan is falsified if daemon mode still relies on `process.sleep_forever()`, if SIGTERM does not call `daemon.shutdown`, if the implementation leaves the default `erl_signal_handler` in place so OTP shuts the VM down before `daemon_shutdown_complete`, if the implementation claims SIGINT/Ctrl-C support, if a graceful stop leaves `instance.lock` or `control.json` behind, if duplicate SIGTERM events cause multiple shutdown attempts, if `--once` or smoke/probe behavior changes, or if the deterministic test suite depends on real OS signal timing.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

The required deterministic tests are lifecycle unit tests, signal-wrapper tests with fake FFI functions, service-level startup-failure cleanup tests, and a service-level graceful-stop test with fake stop messages. Production-path SIGTERM validation is required before accepting the signal bridge, but it may be manual and recorded in Outcomes rather than part of the normal deterministic suite. No SIGINT validation is expected because SIGINT/Ctrl-C is out of scope.

## Validation and Acceptance

Accept this phase when:

- The signal capability spike and SIGTERM-only scope decision are recorded and reflected in this plan, README, and usage text.
- `service.start_daemon` no longer calls `process.sleep_forever()` directly.
- Daemon mode installs a SIGTERM handler and temporarily removes all default `erl_signal_handler` registrations so OTP does not preempt graceful cleanup.
- The signal cleanup path removes the custom handler and restores exactly one `erl_signal_handler` on graceful shutdown and startup failure branches.
- A SIGTERM stop event calls `daemon.shutdown` exactly once.
- Stop-source cleanup runs exactly once on graceful shutdown and startup failure branches.
- The instance lock is released after graceful shutdown.
- The control file is removed after graceful shutdown.
- Workers are stopped by the existing daemon shutdown path.
- README no longer describes SIGTERM as unsupported, accurately states that Ctrl-C/SIGINT remains abrupt, and still warns about `kill -9` and VM crash.
- A production-path SIGTERM validation has been recorded, including the BEAM OS pid, exit status 0 for graceful SIGTERM, and evidence that no stale lock or control file remains.
- The full deterministic suite passes.

## Rollout, Recovery, and Idempotence

This phase is additive to normal daemon behavior except for the deliberate daemon-mode replacement of OTP's default SIGTERM handler registrations. If signal installation fails at startup, daemon startup should fail with a clear `signal_handler_failed` error rather than running in a mode that claims graceful shutdown but cannot handle signals. Because the stop source is installed before `daemon.start`, every startup failure branch after installation must call the cleanup function before releasing the instance lock and must restore the default `erl_signal_handler` if it was replaced and is not already registered.

If graceful shutdown times out, Scherzo should log the timeout and exit nonzero. The timeout policy intentionally releases the local lock as part of CLI exit; record this decision because it assumes `src/scherzo/main.gleam` will immediately halt the VM after the error. Operators may still need to verify processes and remove stale locks manually in that rare case.

Repeated SIGTERM stop events during one shutdown should not re-run cleanup. The first SIGTERM starts shutdown; later SIGTERM events are ignored or logged by the lifecycle layer and ignored by the FFI handler after its delivered flag is set. Documentation must not imply that Ctrl-C/SIGINT follows this graceful path.

## Artifacts and Notes

Target shutdown transcript shape:

    level=info service=scherzo event=signal_handler_installed signal=sigterm os_pid=12345
    level=info service=scherzo event=daemon_stop_requested reason=sigterm
    level=info service=scherzo event=daemon_shutdown
    level=info service=scherzo event=daemon_shutdown_complete

Signal spike transcript recorded before implementation:

    sigint: {'EXIT',{badarg,...}}
    sigterm: ok

Signal-handler replacement evidence recorded during plan review:

    gen_event:which_handlers(erl_signal_server) -> [erl_signal_handler,prim_tty_sighandler]
    custom handler plus default erl_signal_handler -> VM exited before a 3s grace period completed
    custom handler after deleting erl_signal_handler -> 3s grace period completed and post-sleep marker was written
    gen_event:add_handler(erl_signal_server, erl_signal_handler, []) -> ok

A later hardening plan will address crash recovery and durable state. This plan only handles graceful stop requests while the BEAM VM is alive and able to run cleanup code.

## Interfaces and Dependencies

In `src/scherzo/lifecycle.gleam`, expose functions equivalent to:

    pub type StopReason {
      Sigterm
      TestStop(String)
    }

    pub type ShutdownResult {
      ShutdownComplete
      ShutdownTimedOut
    }

    pub fn run_until_stop(
      stop_subject: process.Subject(StopReason),
      shutdown: fn(StopReason) -> Result(Nil, Nil),
      cleanup_stop_source: fn() -> Nil,
      release: fn() -> Nil,
      logger: fn(String, String, List(log.Field)) -> Nil,
    ) -> ShutdownResult

The exact signature may differ to fit Gleam process APIs, but tests must be able to inject stop messages and fake shutdown, cleanup, and release functions.

In `src/scherzo/signal.gleam`, expose functions equivalent to:

    type SignalHandle

    pub type Installation {
      Installation(
        cleanup: fn() -> Nil,
        installed_signals: List(lifecycle.StopReason),
        os_pid: String,
      )
    }

    pub fn install(
      subject: process.Subject(lifecycle.StopReason),
    ) -> Result(Installation, String)

    pub fn install_with_ffi(
      subject: process.Subject(lifecycle.StopReason),
      ffi_install: fn(process.Subject(lifecycle.StopReason)) -> Result(#(handle, String), String),
      ffi_cleanup: fn(handle) -> Nil,
    ) -> Result(Installation, String)

The production `install` calls `install_with_ffi(subject, ffi_install_sigterm, ffi_cleanup_sigterm)`, where `SignalHandle` is the private production FFI handle type. The test-visible helper is generic over the fake handle type, so deterministic tests can exercise success, failure, metadata, and idempotent cleanup without changing the VM's real signal handlers.

In `src/scherzo/orchestrator/service.gleam`, expose a helper equivalent to:

    pub type DaemonLifecycleDependencies {
      DaemonLifecycleDependencies(
        daemon_dependencies: daemon.RuntimeDependencies,
        install_stop_source: fn(process.Subject(lifecycle.StopReason)) -> Result(signal.Installation, String),
        shutdown_timeout_ms: Int,
        lifecycle_logger: fn(String, String, List(log.Field)) -> Nil,
      )
    }

    pub fn start_daemon_with_lifecycle(
      workflow_path: Option(String),
      dependencies: DaemonLifecycleDependencies,
    ) -> Result(Nil, StartupError)

No new package dependency should be required. Add only a small Erlang FFI module for signal handling. The FFI must fail clearly when SIGTERM cannot be installed, when the `erl_signal_server` gen_event manager is unavailable, or when the custom handler cannot replace `erl_signal_handler`. The FFI must restore the default handler on cleanup without registering duplicate `erl_signal_handler` handlers, and must not touch SIGINT/Ctrl-C handling.
