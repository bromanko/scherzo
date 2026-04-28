# Make Scherzo ready for one real Linear board

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, an operator can run Scherzo as a long-lived daemon against one private Linear project and observe it polling real Linear over HTTPS, reconciling active runs, preparing per-issue workspaces, probing and running pi in those workspaces, retrying failures with real timers, reloading `WORKFLOW.md` changes, optionally reporting handoff information back to Linear, and stopping live workers through the daemon shutdown API without leaving daemon-owned pi ports open. The visible proof is that `direnv exec . gleam run -- --linear-smoke path/to/WORKFLOW.md` performs a bounded, read-only, credential-gated Linear check; `direnv exec . gleam run -- --pi-probe path/to/WORKFLOW.md` launches real pi without sending a prompt; and `direnv exec . gleam run -- path/to/WORKFLOW.md` blocks as a daemon that logs repeated poll cycles until the OS process is terminated.

This pass deliberately narrows the shutdown claim. Programmatic shutdown through `daemon.shutdown` must cancel timers, stop monitored worker processes, and close the pi ports owned by those worker processes. The CLI path does not yet install SIGINT or SIGTERM handlers. When the operator terminates the VM process from the shell, Scherzo relies on Erlang port ownership and OS process teardown for child cleanup, and the local instance-lock file may remain as a stale lock that must be removed manually after verifying no Scherzo process is active. Adding explicit signal trapping is a future hardening task unless this plan is revised again to include it.

This plan targets safe real-board readiness for one Scherzo instance per Linear project and canonical workspace root. It does not promise multi-host exactly-once dispatch. It adds a local workspace-root instance lock and optional Linear comments/state updates for operator-visible claiming and handoff, but a future durable distributed claim backend is still required before multiple hosts or multiple independent workspace roots may process the same Linear project.

## Problem Framing and Constraints

The current repository has strong deterministic foundations but still cannot be trusted as an unattended board runner. `src/scherzo/orchestrator/service.gleam` runs one synchronous tick. `default_dependencies()` now constructs a real Linear tracker through private service-local HTTP helpers, but the real transport is not reusable from `src/scherzo/linear.gleam`, there is no read-only smoke command, and CLI startup still returns after the one-tick harness. `src/scherzo/config.gleam` already has workflow reload state, but no live loop uses it. Retry entries produce pure effects but no real timers. `src/scherzo/agent/pi_rpc.gleam` now has a whole-turn deadline, but short `pi.read_timeout_ms` timeouts still fail an otherwise healthy long turn and `pi.stall_timeout_ms` is not enforced. Linear writes and distributed protection are intentionally absent, so a human looking at Linear cannot reliably tell what Scherzo claimed or how a run ended unless the pi agent itself updates the issue.

The goal is not to turn Scherzo into a general distributed job system or dashboard. The goal is to make the existing architecture real for one board: use the already declared Gleam/Erlang dependencies, preserve deterministic tests, keep pi execution confined to prepared workspaces, route pi event data through one daemon seam for future subscribers, keep secrets out of logs, and provide explicit smoke/probe modes before any prompt is sent to a real pi process. The implementation must remain a Gleam Erlang-target application run through the existing `devenv` and `direnv` workflow.

## Strategy Overview

First make the current tree honest and safe before adding more runtime behavior. Fix the pure retry/continuation semantics so a self-claimed issue with a retry entry can actually dispatch again, and split configuration resolution from dispatch-hook validation so read-only smoke does not require workspace hooks. Then promote the existing service-local Linear HTTP transport into `src/scherzo/linear.gleam` and add a bounded read-only smoke path. Then make startup choose between `--once`, `--linear-smoke`, `--pi-probe`, and daemon mode with structured redacted stderr logging.

After those outer boundaries are stable, add a long-lived daemon actor that owns polling, workflow reload state, retry timers, worker handles, handoff, and shutdown. The daemon is the only component that mutates `domain.RuntimeState`. Workers are monitored, not merely spawned. Every pi update emitted by the runner is sent back to the daemon as `WorkerUpdate` and logged there, even if Phase 1 has no EventHub yet. Worker handles are intentionally extensible: they store pid, monitor, workspace path, run ID, and an optional command subject reserved for future commands such as abort, queue prompt, respond to UI request, and stop after the current turn. Existing Phase 1 behavior may leave the command subject as `None`, but daemon code must call helper functions such as `stop_worker(handle)` instead of assuming “pid only” everywhere.

Finally harden pi timeouts and probes, then add optional Linear handoff writes and a canonical workspace-root instance lock. The design reuses the existing pure core, runner, workspace manager, Linear request builder, and fake-pi tests. It avoids a database, HTTP dashboard, durable scheduler state, startup scans of all terminal Linear issues, and name-based Linear state lookup until there is evidence they are necessary.

## Alternatives Considered

The simplest alternative is to leave `service.gleam` as a one-tick harness and only expose the existing real Linear HTTP path. That would prove real Linear reads but would still not continuously poll, retry, reload, reconcile, or stop workers through a daemon API, so it would not solve the operator problem.

Another alternative is to implement a full distributed claim backend before any real-board run. That is safer for multi-host operation but larger than the immediate problem. This plan instead makes one-instance operation honest with a canonical local lock, Linear-visible handoff, and documentation that multi-host use remains unsupported.

A third alternative is to implement SIGINT/SIGTERM trapping now. That would make CLI shutdown cleaner, but it adds another platform-specific Erlang FFI surface. This plan keeps Phase 1 smaller by testing graceful programmatic daemon shutdown and documenting the current CLI termination caveat. If stakeholders require Ctrl-C cleanup as a hard acceptance gate, add a separate signal-handling milestone before implementation begins.

A fourth alternative is to make Scherzo infer Linear state IDs by state name and automatically move issues through states. That adds API queries and ambiguity around teams and workflows. This plan uses explicit optional state IDs in `WORKFLOW.md`, with comments enabled independently, so an operator can start with comments only and add state transitions after validating IDs in a test project.

## Risks and Countermeasures

The main correctness risk is retry dispatch. Current pure state keeps a claim while a retry is pending, and `core.should_dispatch` rejects claimed issues. If the daemon interprets retry timers without changing this, every failed or continuation issue can become permanently self-blocked. The first implementation milestone must fix and test self-claim retry semantics before the daemon exists.

The main worker-lifecycle risk is assuming `WorkerFinished` always arrives. A worker process may crash before sending its result. The daemon must monitor each worker process, handle `WorkerDown` messages through the same failure-scheduling path as a worker failure, and ignore stale `WorkerFinished` or monitor messages after a handle is removed. Shutdown and reconciliation must stop workers through a single helper that can later prefer a command subject over killing a pid.

The main observability risk is losing pi event data. The runner already accepts `emit_update`; daemon dispatch must wire that callback to send `WorkerUpdate(issue_id, update)` into the daemon mailbox. The daemon must log every update with issue ID, event name, and a truncated/redacted message. This creates the Phase 2 seam for an EventHub without changing runner behavior later.

The main external-service risk is leaking the Linear API key through HTTP errors, mutation payloads, or logs. Real transport errors must never include request headers or bodies. Daemon logging must be structured: callers pass level, event, fields, and current secrets to one logger function that applies `log.format` with the current secret list. Reload must register newly resolved secrets before logging reload-derived details.

The main Linear scale risk is broad terminal scans. The read-only smoke command must use bounded sample reads, not unbounded pagination through all terminal issues. Startup cleanup must not query every terminal issue in the project. In Phase 1, startup cleanup is limited to local Scherzo state such as stale population markers or explicitly known worker state during a live daemon; terminal cleanup happens through live reconciliation of running issues, not by scanning a large historical board.

The main pi risk is distinguishing short poll timeouts from real stalls. The pi loop must treat `pi.read_timeout_ms` as a polling interval while a turn is active, fail with `PiStallTimeout` only after no valid line arrives before `pi.stall_timeout_ms`, and fail with `PiTurnTimeout` when the whole-turn deadline expires before `agent_end`. Command/response reads such as launch and stats may still use `PiReadTimeout` because a short response is required there.

The main reload risk is dispatching from stale policy after the operator saves an invalid workflow, or missing a fast same-size edit. The daemon must compare the workflow file contents, not only second-level mtime and size. On invalid reload, it keeps the last known good config only for reconciliation and already-running workers, blocks all new dispatch including retry dispatch, and keeps polling so a later valid edit can resume work.

The main operational risk is assuming the local instance lock is a distributed lock. It is not. The lock file under the canonical workspace root’s `.scherzo-state/instance.lock` only prevents another Scherzo process using the same canonical workspace root on the same filesystem from starting normally. README and startup logs must state that multiple hosts or different workspace roots against the same Linear project are unsupported until a durable claim backend exists.

## Progress

- [x] (2026-04-28 18:09Z) Read the current-state note that listed the remaining real-board gaps.
- [x] (2026-04-28 18:09Z) Verified the current tree contains `gleam.toml`, runtime modules under `src/scherzo/`, tests under `test/`, `README.md`, `examples/WORKFLOW.md`, and the prior plan at `docs/plans/implement-scherzo.md`.
- [x] (2026-04-28 18:09Z) Re-read the current `service`, `linear`, `tracker`, `main`, `core`, `runner`, `pi_rpc`, `workspace`, `hooks`, `config`, and relevant test files before writing the original plan.
- [x] (2026-04-28 18:09Z) Ran original baseline validation: `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, and `direnv exec . gleam run -- --help`; the test run then reported `79 passed, no failures`.
- [x] (2026-04-28 20:10Z) Re-reviewed the plan against the current tree and found the baseline has advanced to `81 passed, no failures`, `gleam_http` is already direct, service-local real Linear HTTP exists, and pi turn timeout is partially implemented.
- [x] (2026-04-28 20:10Z) Revised this plan to address retry self-claim semantics, smoke/config separation, bounded Linear smoke, monitored workers, WorkerUpdate routing, extensible worker handles, content-based reloads, canonical locks, and the narrowed CLI shutdown claim.
- [x] (2026-04-28 21:05Z) Ran implementation baseline: `direnv exec . gleam test` reported `81 passed, no failures`.
- [x] (2026-04-28 21:05Z) Fixed pure retry self-claim semantics and split dispatch validation from config resolution; deterministic tests cover failed-worker and continuation retries.
- [x] (2026-04-28 21:05Z) Promoted real Linear HTTP transport into `src/scherzo/linear.gleam` and added bounded read-only Linear smoke support.
- [x] (2026-04-28 21:05Z) Added CLI run modes for daemon default, `--once`, `--linear-smoke`, and `--pi-probe`, with structured stderr logging and secret redaction.
- [x] (2026-04-28 21:05Z) Added the long-lived daemon actor with poll, reconciliation, reload, retry, WorkerUpdate logging, monitored workers, programmatic shutdown, and deterministic timer seams.
- [x] (2026-04-28 21:05Z) Hardened pi turn/stall timeout behavior and added the no-prompt pi probe command.
- [x] (2026-04-28 21:05Z) Added optional Linear handoff writes and a canonical workspace-root instance lock.
- [x] (2026-04-28 21:05Z) Updated README and `examples/WORKFLOW.md`; final deterministic validation passed with `101 passed, no failures` and updated help text.
- [x] (2026-04-28 21:16Z) Ran credential-gated read-only Linear smoke with `LINEAR_API_KEY` and `examples/WORKFLOW.md`; after fixing the Authorization header shape, it logged `linear_smoke_ok candidate_count=0 terminal_count=0 refreshed_count=0`.
- [x] (2026-04-28 21:43Z) Created an ignored temporary workflow for Linear project `scherzo-c55181aee33a`, discovered the project then had one `Backlog` issue, reran smoke with `active_states: [Backlog]`, and observed `linear_smoke_ok candidate_count=1 terminal_count=0 refreshed_count=1`.
- [x] (2026-04-28 21:43Z) Ran `--pi-probe` with the fake pi RPC fixture and the real Linear project workflow; transcript contained `set_session_name,set_auto_retry,get_state,get_session_stats` and no `prompt`.
- [x] (2026-04-28 21:43Z) Ran daemon mode with fake pi against the real Linear project workflow using the temporary Backlog validation setup; it fetched one candidate, dispatched one worker, routed `pi_event` logs, exited the worker normally, and parked the active issue at the configured one-session cap without Linear writes.
- [x] (2026-04-28 21:48Z) Ran real `--pi-probe` using `pi --mode rpc --no-session`; it returned `pi_probe_ok` without sending a prompt.
- [x] (2026-04-28 22:02Z) After `LIV-7` was assigned to the Scherzo project in Linear, reran the policy-correct `active_states: [Todo]` smoke and observed `linear_smoke_ok candidate_count=1 terminal_count=0 refreshed_count=1`.
- [x] (2026-04-28 22:02Z) Ran fake-pi daemon dispatch against real Linear issue `LIV-7` in `Todo`; the daemon logged `dispatch_started`, `pi_event`, `worker_exited`, `issue_parked`, and `claim_released`, and the fake-pi transcript included a `prompt` command.
- [x] (2026-04-28 22:06Z) Ran comments-only handoff with fake pi against real Linear issue `LIV-7`; Linear comment count increased from 0 to 2 with one claim comment and one success comment containing the same Scherzo run ID, and the issue remained in `Todo` because no state IDs were configured.
- [x] (2026-04-28 22:31Z) Ran state-transition handoff with fake pi against real Linear issue `LIV-7`; smoke saw one Todo candidate, claim moved the issue to `In Progress`, success moved it to `Done`, and Linear comment count increased from 2 to 4.
- [x] (2026-04-28 22:45Z) Ran one safe real-pi daemon dispatch against real Linear issue `LIV-8`; daemon logged real pi events and normal worker exit, Linear ended in `Done`, and comments include claim plus success with `7769` total pi tokens.
- [x] (2026-04-28 22:49Z) Ran a clean real-pi daemon dispatch against new real Linear issue `LIV-9`; after moving it from `Backlog` to `Todo` for setup, smoke found one active candidate, daemon completed normally, Linear ended in `Done`, and comments include exactly one claim plus one success with `7767` total pi tokens.
- [ ] Add a future extensible `doctor` command that runs named readiness checks such as workflow/config validation, read-only Linear smoke, no-prompt pi probe, workspace hook validation, instance-lock status, and later control/EventHub health checks, so one command can replace or aggregate one-off smoke/probe flags without losing per-check output.

## Surprises & Discoveries

- Observation: `src/scherzo/orchestrator/service.gleam` currently calls `core.apply_worker_start(state, issue, "")`, so a running entry created by the one-tick harness has an empty workspace path until the worker returns.
  Evidence: The dispatch branch in `dispatch_candidates` passes an empty string to `apply_worker_start`; the runner later returns `WorkerSuccess.workspace_path`.

- Observation: `src/scherzo/orchestrator/service.gleam` no longer uses an empty fake tracker in `default_dependencies()`. It constructs a real Linear tracker with private `linear_tracker_client` and `http_transport` helpers.
  Evidence: `default_dependencies()` sets `tracker: linear_tracker_client`, and the same file imports `gleam/http`, `gleam/http/request`, and `gleam/httpc`.

- Observation: `gleam_http` is already a direct dependency.
  Evidence: `gleam.toml` lists both `gleam_http` and `gleam_httpc` under `[dependencies]`.

- Observation: `src/scherzo/agent/pi_rpc.gleam` no longer ignores `turn_timeout_ms`, but it still treats each `pi.read_timeout_ms` as a hard active-turn failure and does not use `stall_timeout_ms`.
  Evidence: `prompt` passes `monotonic_ms() + turn_timeout_ms` into `read_events_until_agent_end`; `read_turn_line` returns `PiReadTimeout` on a short timeout before the turn deadline, and no stall deadline is passed.

- Observation: `src/scherzo/config.gleam` currently validates dispatch hooks inside `resolve_hooks`, so read-only smoke would fail for a workflow that has valid Linear config but no workspace hooks.
  Evidence: `resolve_hooks` returns `DispatchValidationFailed` unless `hooks.after_create` or `hooks.before_run` is set.

- Observation: the current pure retry path is self-blocking.
  Evidence: `apply_worker_failure` leaves the issue ID in `state.claimed`; `handle_retry_candidate` calls `should_dispatch`; `should_dispatch` rejects any issue ID present in `state.claimed`.

- Observation: service-level `--pi-probe` uses the production stderr logger, so its deterministic test emits a `pi_probe_ok` log line during `gleam test`.
  Evidence: final validation still passed with `101 passed, no failures`; the only extra output was a redacted structured `pi_probe_ok` line for the test scratch workspace.

- Observation: `daemon.start` cannot return `service.StartupError` without creating an import cycle once `service.start_daemon` wires production daemon startup.
  Evidence: `src/scherzo/orchestrator/service.gleam` imports `src/scherzo/orchestrator/daemon.gleam`, so the daemon module defines its own equivalent `StartupError` and service maps it at the boundary.

- Observation: Linear API keys must be sent as the raw `Authorization` header value, not as `Authorization: Bearer <key>`.
  Evidence: the first credential-gated smoke run failed with `linear_api_status`; a direct read-only `viewer` query returned HTTP 400 with the message `It looks like you're trying to use an API key as a Bearer token. Remove the Bearer prefix from the Authorization header.` After changing `src/scherzo/linear.gleam`, `--linear-smoke examples/WORKFLOW.md` logged `linear_smoke_ok`.

- Observation: The real test project `scherzo-c55181aee33a` initially had validation issues in Linear state `Backlog`, not in the intended `Todo` active state.
  Evidence: read-only project issue sampling first returned only Backlog issues; once `LIV-7` was assigned to the Scherzo project and was in `Todo`, the policy-correct smoke reported `candidate_count=1`.

- Observation: The fake-pi daemon validation confirmed the documented CLI shutdown caveat.
  Evidence: the first shell-managed background run timed out and left `workspaces/.scherzo-state/instance.lock`; the stale lock was removed manually after verifying no Scherzo BEAM process remained. A later process-group-managed run terminated cleanly enough for the wrapper to exit, and the validation lock was removed after the run.

- Observation: The pi stall/turn-timeout tests were sensitive to using a 10 ms command-response read timeout before entering the active turn loop.
  Evidence: validation runs returned `PiReadTimeout` before reaching the intended `PiStallTimeout` or `PiTurnTimeout` assertions; increasing those tests' command-response read timeout to 1000 ms preserves the intended active-turn timeout assertions without weakening production timeout behavior.

- Observation: A claim state that is not listed in `tracker.active_states` can cause the daemon to stop a long-running worker during reconciliation.
  Evidence: the first real-pi handoff attempt moved `LIV-8` from `Todo` to `In Progress` via `claim_state_id` while the workflow only listed `active_states: [Todo]`. The next poll reconciled the running issue as non-active and logged `worker_stop_requested reason=non_active` before the real pi worker finished. The successful real-pi validation used `active_states: [Todo, In Progress]` and only configured `success_state_id`.

## Decision Log

- Decision: Default CLI execution will become daemon mode, while `--once` will preserve the current deterministic one-tick behavior.
  Rationale: Operators expect `gleam run -- WORKFLOW.md` to keep polling once Scherzo is real-board ready. Keeping `--once` avoids losing the existing harness and makes smoke testing safe.
  Date: 2026-04-28

- Decision: Promote the existing `gleam_httpc` transport from `src/scherzo/orchestrator/service.gleam` into `src/scherzo/linear.gleam` instead of adding another HTTP client.
  Rationale: The repository already has working service-local HTTP code and direct `gleam_http`/`gleam_httpc` dependencies. Moving it avoids duplication and makes smoke, daemon, and tests share one transport.
  Date: 2026-04-28

- Decision: Support one Scherzo instance per Linear project and canonical workspace root in this pass, not multi-host exactly-once dispatch.
  Rationale: The current orchestrator is intentionally in-memory. A durable distributed claim backend is a larger design. A local lock and Linear-visible handoff reduce real-board risk without misrepresenting the guarantee.
  Date: 2026-04-28

- Decision: Do not implement SIGINT/SIGTERM trapping in Phase 1 unless this plan is revised again.
  Rationale: The immediate safety boundary is programmatic daemon shutdown and explicit documentation of CLI termination behavior. Signal handling is useful but should not be implied by tests that only exercise `daemon.shutdown`.
  Date: 2026-04-28

- Decision: Fix retry self-claim semantics in the pure core before adding daemon retry timers.
  Rationale: A daemon that faithfully interprets current retry effects would never re-dispatch failed or continuation issues because the existing claim blocks `should_dispatch`.
  Date: 2026-04-28

- Decision: Route every pi update through `WorkerUpdate` in the daemon even before adding an EventHub.
  Rationale: This preserves event data and creates the future fan-out seam without changing runner APIs later.
  Date: 2026-04-28

- Decision: Worker handles will store pid, monitor, workspace path, run ID, and an optional command subject.
  Rationale: Phase 1 only needs shutdown and reconciliation stop behavior, but future interaction will need abort, queued prompts, UI responses, and stop-after-current-turn. The type should not bake in a pid-only model.
  Date: 2026-04-28

- Decision: Workflow reload detection will compare file contents, not mtime and size only.
  Rationale: Second-level mtime plus size can miss same-size edits made quickly. `WORKFLOW.md` is small enough to read before each poll.
  Date: 2026-04-28

- Decision: Read-only smoke will use bounded Linear sample reads and will not require dispatch hooks.
  Rationale: Smoke is a credential and API compatibility check, not a workspace dispatch check. It must be safe on real boards with large terminal history.
  Date: 2026-04-28

- Decision: Startup will not scan all terminal Linear issues for cleanup in Phase 1.
  Rationale: That scan is potentially large and cannot reliably distinguish Scherzo-owned local workspaces without durable local state. Live reconciliation still cleans terminal running issues.
  Date: 2026-04-28

- Decision: Linear state transitions will use optional operator-provided state IDs instead of state names.
  Rationale: Linear workflow state names are team-scoped and can be duplicated. State IDs are precise and make the first write path small. Operators can start with comments only.
  Date: 2026-04-28

- Decision: The pi probe command will not send a prompt.
  Rationale: The safest real-pi validation before board use is to prove launch, `set_session_name`, `set_auto_retry`, `get_state`, and `get_session_stats` without spending model tokens or allowing tools to run.
  Date: 2026-04-28

- Decision: `src/scherzo/orchestrator/daemon.gleam` defines a daemon-local `StartupError`, and `src/scherzo/orchestrator/service.gleam` maps it into `service.StartupError`.
  Rationale: Returning `service.StartupError` directly from the daemon would create a circular module dependency once service imports daemon for production startup.
  Date: 2026-04-28

- Decision: `WorkerHandle` stores the original `domain.Issue` in addition to the required issue ID, run ID, pid, monitor, workspace path, and optional command subject.
  Rationale: Handoff success/failure reporting needs stable issue context even after the pure runtime state has removed the running entry.
  Date: 2026-04-28

- Decision: Linear request builders set `Authorization` to the raw API key, not `Bearer <key>`.
  Rationale: Real Linear rejects Bearer-prefixed API keys with HTTP 400, while the raw key header allows the smoke query to succeed.
  Date: 2026-04-28

- Decision: Documentation now warns that `claim_state_id` must move issues into a state listed in `tracker.active_states`, or else operators should omit `claim_state_id` and rely on comments-only claim handoff plus `success_state_id`.
  Rationale: The daemon reconciles running issues by Linear state, so a non-active claim state is interpreted as a reason to stop the worker.
  Date: 2026-04-28

## Outcomes & Retrospective

2026-04-28 implementation outcome: The deterministic portion of the plan is complete. Scherzo now has daemon default CLI behavior, one-shot mode, read-only Linear smoke mode, no-prompt pi probe mode, reusable real Linear transport, bounded smoke readers, an OTP daemon actor, retry timers, content-based reloads, monitored workers, WorkerUpdate logging, pi stall/turn timeout separation, optional Linear handoff, a local instance lock, updated documentation, and deterministic coverage. The final deterministic gate passed: `direnv exec . gleam format --check src test`, `direnv exec . gleam test` with `101 passed, no failures`, and `direnv exec . gleam run -- --help`.

Completion outcome: read-only smoke with the intended `Todo` active state, fake-pi no-prompt probe, fake-pi daemon dispatch against `LIV-7`, comments-only Linear handoff, state-transition Linear handoff, real no-prompt `--pi-probe`, one safe real-pi daemon dispatch against `LIV-8`, and one clean real-pi daemon dispatch against `LIV-9` have all succeeded against the private Linear project slug supplied during validation. Multi-host or multi-workspace-root operation remains unsupported, and CLI SIGINT/SIGTERM still relies on VM/OS teardown rather than graceful `daemon.shutdown`.

## Context and Orientation

Scherzo is a Gleam project targeting Erlang. Package metadata and dependencies are in `gleam.toml`; generated dependency locks are in `manifest.toml`. Runtime source lives under `src/scherzo/`. Tests live under `test/` and are run through `gleeunit` using `direnv exec . gleam test`.

The current runtime entry point is `src/scherzo/main.gleam`. It parses either no arguments, `--help`, or one workflow path, then calls `service.start`. The service code in `src/scherzo/orchestrator/service.gleam` loads a workflow, resolves config, validates dispatch hooks, creates a new pure runtime state with `core.new_state`, fetches candidate issues through an injected `tracker.Client`, and synchronously runs `runner.run_attempt` for dispatchable issues. Its production `default_dependencies()` currently constructs a real Linear tracker through private service-local HTTP code, but tests can still inject fake dependencies through `run_once_with_dependencies`.

The pure scheduler logic is in `src/scherzo/orchestrator/core.gleam`. It decides dispatch eligibility, sorting, retry effects, parking, reconciliation effects, token totals, and claim release. Effects such as `ScheduleRetry`, `CancelRetry`, `CleanupWorkspace`, `StopWorker`, and `Dispatch` are currently pure values; no long-lived actor interprets them yet. The retry self-claim issue described above must be fixed here.

The Linear integration is in `src/scherzo/linear.gleam`. It can build GraphQL request bodies, add Authorization and Content-Type headers to its own `linear.Request` record, paginate fake responses, normalize issues, and map payload/status errors. Real HTTP conversion and dispatch currently live in `src/scherzo/orchestrator/service.gleam`; this plan moves that code into `linear.gleam` and exposes reusable real-client constructors.

The pi runner is in `src/scherzo/agent/runner.gleam`, `src/scherzo/agent/probe.gleam`, and `src/scherzo/agent/pi_rpc.gleam`. It prepares a workspace, renders the prompt, optionally probes pi, sends prompt commands, waits for fake pi events, fetches stats, refreshes issue state through the tracker, and returns success or failure. The runner already accepts `emit_update: fn(String, runner.PiUpdate) -> Nil`; daemon dispatch must use this callback to send `WorkerUpdate` into the actor.

The workspace manager is `src/scherzo/workspace.gleam`. It sanitizes issue identifiers into workspace keys, enforces root containment, creates or reuses workspace directories, runs `hooks.after_create` and `hooks.before_run`, uses `.scherzo-state/<key>.populating` sidecar markers for partial population, and can cleanup a stored workspace path. Hooks are implemented in `src/scherzo/hooks.gleam` using the port wrapper.

Configuration is resolved in `src/scherzo/config.gleam`. It defines defaults for Linear endpoint, polling interval, workspace root, hooks, agent limits, and pi settings. It already has `ReloadState`, `apply_reload`, `can_dispatch`, and `resolved_secrets`, but those are not yet used by a live loop. This plan changes hook resolution so missing dispatch hooks can be represented in config, while `validate_dispatch` remains the gate for modes that may dispatch.

## Preconditions and Verified Facts

The current baseline commands from the repository root are:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -- --help

On 2026-04-28 after plan review, `direnv exec . gleam test` ended with `81 passed, no failures`. The help command still prints usage for `gleam run -- [path-to-WORKFLOW.md]`; it does not yet document `--once`, `--linear-smoke`, or `--pi-probe`.

`gleam.toml` currently declares these runtime dependencies: `gleam_stdlib`, `gleam_erlang`, `gleam_otp`, `gleam_json`, `gleam_http`, `gleam_httpc`, `simplifile`, `yay`, and `birl`. No new package dependency is required by this plan. New runtime FFI modules may be added under `src/` for the instance lock and any process-tree test support if needed.

The existing tests use direct `domain.EffectiveConfig` constructors in several files. Adding a new top-level `handoff` field to `domain.EffectiveConfig` will require updating test helper constructors in `test/orchestrator_core_test.gleam`, `test/agent_runner_test.gleam`, and any new tests that build configs directly.

Real Linear smoke validation requires `LINEAR_API_KEY` and a workflow with `tracker.project_slug`. The smoke command must be read-only and must not dispatch pi or require workspace hooks. Real pi probe validation requires a `pi.command` that can run in RPC mode and a workflow whose workspace hooks can safely prepare a scratch workspace. No test in `direnv exec . gleam test` may require real Linear credentials, real network access, or a real pi binary.

## Scope Boundaries

In scope: fixing pure retry self-claim semantics; separating config resolution from dispatch validation; reusable real Linear HTTP POST transport in `linear.gleam`; production `tracker.Client` construction from `domain.TrackerConfig`; bounded read-only Linear smoke command; CLI modes for daemon, once, Linear smoke, and pi probe; structured stderr logging with current-secret redaction; a long-lived OTP actor; deterministic timer seams for tests; recurring poll timers in production; retry timers with generation checks and cancellation; candidate dispatch in the live loop; running issue reconciliation; content-based workflow reload; invalid reload dispatch blocking; monitored worker processes; `WorkerUpdate` routing and logging; programmatic daemon shutdown that cancels timers and terminates live worker processes; pi turn timeout and stall timeout enforcement; no-prompt real pi probe including session stats shape; optional Linear handoff comments and optional state updates by configured state ID; a canonical workspace-root instance lock; README and example workflow updates; deterministic tests for all new behavior.

Out of scope: CLI SIGINT/SIGTERM trapping; a web dashboard; EventHub fan-out beyond daemon logging; JSON snapshot API; SSH workers; the optional `linear_graphql` pi extension; a multi-host distributed lock; durable scheduler state; startup scans of all terminal Linear issues; recovery of a live pi session after BEAM process restart; automatic discovery of Linear workflow state IDs by name; automatic repository checkout beyond workflow hooks; and production packaging beyond `gleam run` and existing Gleam export capability.

Every retained user-visible surface must keep working: `direnv exec . gleam test`, `direnv exec . gleam run -- --help`, deterministic fake-pi tests, `service.run_once_with_dependencies` for tests, the existing workflow schema keys, and `agent.max_concurrent_agents: 0` as a pause for new dispatch.

## Milestones

Milestone 0 refreshes and fixes current foundations. At the end, config resolution can represent missing dispatch hooks, `validate_dispatch` remains the mode-specific gate for dispatching modes, and core retry/continuation dispatch can proceed for a self-claimed issue with a current retry entry. This comes first because the daemon must not encode workarounds for broken pure semantics.

Milestone 1 adds reusable real Linear read transport and bounded smoke support without changing daemon behavior. At the end, `linear.http_transport` can send a POST through `gleam_httpc`, `linear.real_client` returns a `tracker.Client`, and `scherzo/smoke.gleam` can run a bounded read-only Linear smoke check through injected sample readers. This validates real network reads without pi dispatch.

Milestone 2 makes startup construct real dependencies and exposes safe CLI modes. At the end, `--once` runs the current harness with real dependencies by default, `--linear-smoke` performs only bounded read-only tracker calls, `--help` documents all modes, logs go to stderr with current-secret redaction, and daemon mode has a placeholder only until Milestone 3 completes.

Milestone 3 implements the long-lived daemon actor. At the end, daemon mode blocks, schedules recurring poll ticks, reconciles running issues on each tick, interprets retry timer effects, reloads workflow changes by content, blocks dispatch while the current workflow is invalid, routes every `WorkerUpdate` through daemon logging, monitors workers, handles abnormal worker exits, and supports programmatic shutdown by cancelling timers and stopping live workers. This milestone turns the already-tested core transitions into real runtime behavior.

Milestone 4 hardens pi runtime behavior and adds the no-prompt pi probe. At the end, `pi.turn_timeout_ms` and `pi.stall_timeout_ms` are enforced for active turns, short `pi.read_timeout_ms` polls no longer fail a healthy long turn, `probe.probe` verifies `get_session_stats` without sending a prompt, and `--pi-probe` prepares a scratch workspace, runs the probe, and cleans it up.

Milestone 5 adds Linear-visible handoff and local single-instance protection. At the end, optional `handoff` workflow config can create Linear comments on claim/success/failure and optionally move issues to configured Linear state IDs, while `instance_lock.acquire` prevents two Scherzo processes using the same canonical workspace root from running at the same time. This is one-instance protection, not a distributed exactly-once claim.

Milestone 6 updates documentation and performs staged acceptance. At the end, README and `examples/WORKFLOW.md` describe new modes, the signal-handling caveat, handoff settings, lock behavior, stale lock recovery, and the continued prohibition on multiple hosts or multiple workspace roots for the same Linear project. Deterministic validation passes, read-only smoke succeeds against a private test project when credentials are supplied, fake-pi daemon dispatch succeeds against real Linear in a test project, and one safe real-pi issue can be attempted only after smoke and probe pass.

## Plan of Work

Start by updating tests around the pure core. In `test/orchestrator_core_test.gleam`, add a test that starts an issue, applies `core.apply_worker_failure`, then calls `core.handle_retry_candidate` with `Ok(Some(updated_issue))` and asserts the transition emits `Dispatch(updated_issue)` instead of scheduling another retry solely because the issue is claimed. Add the same assertion for the continuation path after `apply_worker_success_with_workspace_path` returns `ScheduleRetry`. Implement this by adding a private core predicate for retry dispatch that allows an existing claim for the same issue only when a retry entry for that issue exists and the issue is not running. Do not weaken normal `should_dispatch`; candidate dispatch must still reject already claimed issues.

Then split dispatch-hook validation from config resolution. In `src/scherzo/config.gleam`, change `resolve_hooks` so it returns a `HooksConfig` with `None` hook fields when hooks are absent. Keep positive timeout validation. Keep `validate_dispatch` as the single check that at least one of `hooks.after_create` or `hooks.before_run` is present. Update existing tests to call `validate_dispatch` where they expect dispatch validation. Add a smoke-specific test that a workflow with Linear config but no hooks resolves successfully and that `validate_dispatch` fails on the same config.

Move real HTTP support from `src/scherzo/orchestrator/service.gleam` into `src/scherzo/linear.gleam` rather than creating a parallel Linear module. Keep the existing `Transport` type so tests can continue to inject fake responses. Add conversion helpers from `linear.Request` to `gleam/http/request.Request(String)`, a `http_transport` function that dispatches with `gleam_httpc.timeout(request.timeout_ms)`, and client constructors that close over a config and transport. Remove the duplicate private HTTP code from service after tests pass.

Add bounded smoke support in `src/scherzo/smoke.gleam`. Smoke should not dispatch, prepare workspaces, require hooks, or send pi prompts. It should accept a small `LinearSmokeReader` record with `fetch_candidate_sample`, `fetch_terminal_sample`, and `refresh_issue_states_by_ids` functions so tests use fake readers and production uses Linear sample queries. Production smoke should request at most one page each for active and terminal states. If no issue exists in the samples, refresh count is zero and smoke still succeeds.

Refactor `src/scherzo/orchestrator/service.gleam` carefully. Preserve `Dependencies` and `run_once_with_dependencies` for deterministic tests. Add production helpers for once, smoke, daemon, and pi-probe modes. The production logger should be structured: a function receives level, event, fields, and current secrets, formats with `log.format`, and writes to stderr through `gleam/io.println_error`. Do not let daemon code preformat log lines without secrets.

Create `src/scherzo/orchestrator/daemon.gleam` for the actor rather than overloading the synchronous harness. The daemon actor state should contain the workflow path, last workflow file contents, current workflow definition, reload state, current effective config, current tracker client, current handoff client, runtime state, poll timer, retry timers by issue ID, worker handles by issue ID, current resolved secrets, and runtime dependencies.

The daemon message type must include `PollTick(Int)`, `RetryTick(String, Int)`, `WorkerFinished(String, String, Result(runner.WorkerSuccess, runner.WorkerFailure))`, `WorkerUpdate(String, runner.PiUpdate)`, `WorkerDown(process.Down)`, `Shutdown(process.Subject(Nil))`, and `GetSnapshot(process.Subject(domain.RuntimeState))`. The second `String` in `WorkerFinished` is the run ID. `WorkerDown` is produced by a selector using `process.select_monitors`.

Define a `WorkerCommand` type now even if no command is sent in Phase 1. Include variants such as `Abort`, `StopAfterCurrentTurn`, `QueuePrompt(String)`, and `RespondToUi(String, String)` as placeholders for future interaction. Define `WorkerHandle` with at least `issue_id`, `run_id`, `pid`, `monitor`, `workspace_path`, and `command_subject: Option(process.Subject(WorkerCommand))`. All daemon code that stops a worker must call `stop_worker(handle)` so a future command subject can be used before falling back to `process.kill(handle.pid)`.

Interpret core effects in one place in the daemon. `Dispatch(issue)` calls the same dispatch helper after rechecking eligibility with the retry-aware path when it came from a retry tick. `ScheduleRetry(issue_id, delay_ms, generation, reason)` stores a timer for `RetryTick(issue_id, generation)`. `CancelRetry(issue_id)` cancels and removes any stored timer. `CleanupWorkspace(path)` calls the cleanup dependency only when `path` is non-empty and logs any cleanup error. `StopWorker(issue_id, reason)` calls `stop_worker` if a handle is present and removes it. `ReleaseClaim` and `ParkIssue` primarily log because the pure core has already changed state.

Use injectable timer functions in daemon dependencies so tests can be deterministic. Production dependencies wrap `process.send_after` and timer cancellation. Tests may use a fake timer handle and send `PollTick` or `RetryTick` messages manually. Keep one small integration test with a real short timer if desired, but do not make core daemon correctness depend on sleeps.

Implement workflow reload by reading the workflow file contents before each poll tick and comparing them to the last stored contents. On changed contents, parse with `workflow.parse`, resolve with `config.resolve`, call `validate_dispatch` only for dispatching modes, update `config.ReloadState`, rebuild tracker and handoff clients on success, update secrets before logging reload details, and block candidate and retry dispatch on failure. Running worker reconciliation continues using the last known good config.

Dispatch must compute the deterministic workspace path with `workspace.workspace_path(config.workspace.root, issue.identifier)` before spawning. Pass that path to `core.apply_worker_start`. The worker still calls `workspace.prepare`; computing the path early is for reconciliation, shutdown, cleanup effects, and handoff logs. If path computation fails, log `dispatch_workspace_path_failed` and do not spawn a worker.

When spawning a worker, generate a run ID from the issue identifier plus deterministic daemon data such as `now_ms()` and the current attempt counter. Run ID generation must be in one helper so tests can inject `now_ms` and assert comments/logs exactly. The worker process runs the injected `agent_runner`; its `emit_update` callback sends `WorkerUpdate(issue_id, update)` to the daemon subject; on normal return it sends `WorkerFinished(issue_id, run_id, result)`. The daemon monitors the worker pid immediately after spawning. If `WorkerFinished` arrives first, handle success/failure, remove the worker handle, demonitor if practical, and ignore the later stale monitor message. If `WorkerDown` arrives first for a known handle, treat it as a worker failure with an abnormal-exit reason and apply retry/park logic.

Add pi timeout behavior in `src/scherzo/agent/pi_rpc.gleam`. Keep strict `PiReadTimeout` for command-response reads. For active prompt turns, replace the current single blocking read outcome with a loop that repeatedly reads with `read_timeout_ms`, treats `port.ReadTimeout` as a poll timeout, checks elapsed time against both the whole-turn deadline and the stall deadline, resets the stall deadline whenever a valid response or event line is received, and returns `PiStallTimeout` when no valid line arrives before the stall deadline.

Add `src/scherzo/handoff.gleam` and extend `domain.EffectiveConfig` with a `handoff` field. Handoff is disabled by default. When enabled, the daemon calls `handoff.claim_issue` before spawning a worker, `handoff.report_success` after worker success, and `handoff.report_failure` after worker failure. Handoff failures must be logged and must not crash the daemon. Claim failure prevents dispatch for that tick because the operator-visible claim did not land. Handoff comments are append-only in Phase 1; they must include the run ID so duplicate comments caused by crash/retry are correlatable rather than ambiguous.

Add `src/scherzo/instance_lock.gleam` and `src/scherzo_lock_ffi.erl`. Canonicalize the workspace root with `path.absolute` before building `canonical_root/.scherzo-state/instance.lock`. Acquiring the lock creates the state directory and opens the lock file with exclusive creation. The file contains a short message with the current OS pid or BEAM pid if available, plus the canonical workspace root. Releasing the lock closes and deletes it. If acquire fails because the file exists, startup fails with a redacted, operator-visible message explaining that another Scherzo process may be running or a stale lock must be removed manually after verifying no process is active.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current pass count in Progress. The expected current output before implementation is `81 passed, no failures`.

2. In `test/orchestrator_core_test.gleam`, add `retry_candidate_can_dispatch_self_claimed_issue_test`. Build a config with one slot, start issue `ABC-1`, call `core.apply_worker_failure`, call `core.handle_retry_candidate` with `Ok(Some(updated_issue))`, and assert the effects are `[core.Dispatch(updated_issue)]`. Run `direnv exec . gleam test`; this test should fail before the core change because the current code schedules another retry.

3. In `test/orchestrator_core_test.gleam`, add `continuation_retry_can_dispatch_self_claimed_issue_test`. Make `apply_worker_success_with_workspace_path` finish with an active issue and produce a continuation retry, then feed the same active issue through `handle_retry_candidate` and assert it emits `Dispatch`.

4. In `src/scherzo/orchestrator/core.gleam`, implement retry-aware dispatch eligibility without changing normal candidate dispatch. A private helper should allow the existing claim only when `state.retry_attempts` contains the same issue ID, while still rejecting running issues, parked stale issues, full slots, blockers, non-active states, and terminal states.

5. Run `direnv exec . gleam test`. The new core tests must pass, and all existing tests must still pass.

6. In `test/config_test.gleam`, add `resolve_allows_missing_hooks_but_validate_dispatch_fails_test`. Use a workflow with valid tracker config and no `hooks.after_create` or `hooks.before_run`; assert `config.resolve_with_env` returns `Ok(config)` and `config.validate_dispatch(config)` returns `DispatchValidationFailed`.

7. In `src/scherzo/config.gleam`, change `resolve_hooks` to return absent hooks instead of failing dispatch validation. Keep `hooks.timeout_ms` validation. Ensure all production dispatching modes call `validate_dispatch` before dispatch or workspace preparation.

8. Run `direnv exec . gleam test` and confirm config tests pass.

9. Create `test/linear_http_test.gleam`. Add `real_client_delegates_candidate_terminal_and_refresh_queries_test` that builds a `domain.TrackerConfig`, injects a fake `linear.Transport` returning the existing fake page body shape from `test/linear_test.gleam`, calls `linear.client(config, transport)`, and asserts that `fetch_candidate_issues`, `fetch_issues_by_states(["Done"])`, and `fetch_issue_states_by_ids(["issue-id"])` each return normalized issues.

10. In `test/linear_http_test.gleam`, add `http_transport_maps_httpc_errors_without_secret_values_test`. Exercise an exported pure helper such as `linear.http_error_to_string` with representative constructible `gleam_httpc` errors and assert returned strings do not contain an API key value such as `secret-key`.

11. In `src/scherzo/linear.gleam`, import `gleam/http`, `gleam/http/request`, `gleam/httpc`, and `scherzo/tracker`. Add `pub fn client(config: domain.TrackerConfig, transport: Transport) -> tracker.Client` and `pub fn real_client(config: domain.TrackerConfig) -> tracker.Client`.

12. In `src/scherzo/linear.gleam`, add `pub fn http_transport(request: Request) -> Result(Response, error.TrackerError)`. Convert the endpoint, set method to `http.Post`, set the body and headers, configure `httpc.timeout(request.timeout_ms)`, dispatch, map success to `linear.Response`, and map URL/HTTP errors to generic `error.LinearApiRequest` messages that never include headers or body.

13. Remove the private HTTP conversion helpers from `src/scherzo/orchestrator/service.gleam` and have service production dependencies call `linear.real_client` instead.

14. Run `direnv exec . gleam test`; the Linear tests should pass.

15. Create `src/scherzo/smoke.gleam`. Define `LinearSmokeReader` and `LinearSmokeResult(candidate_count, terminal_count, refreshed_count)`. Implement `linear_read_smoke(reader, terminal_states)` by fetching bounded candidate and terminal samples and refreshing the first sampled issue ID if any exists.

16. Add production smoke reader helpers that use at most one Linear page for active states and at most one Linear page for terminal states. Do not call unbounded `fetch_pages` from smoke.

17. Create `test/smoke_test.gleam`. Add one test where the fake reader returns one candidate and one terminal issue and assert counts `1`, `1`, and `1`. Add one test where the fake reader returns no issues and assert all counts are zero. Add one test that proves the fake reader is called once for candidates and once for terminals, not paginated repeatedly.

18. Run `direnv exec . gleam test` and expect all tests to pass.

19. In `src/scherzo/main.gleam`, replace `CliResult` with a result that includes a `RunMode` of `Daemon`, `Once`, `LinearSmoke`, or `PiProbe`. Update `parse_args` so `[]` and `[path]` mean daemon mode, `--once [path]` means one tick, `--linear-smoke [path]` means smoke only, `--pi-probe [path]` means no-prompt pi probe, and `--help` or `-h` prints usage. Treat unknown flags or too many args as usage errors with exit code `2`.

20. Update `test/main_test.gleam` to assert the new parse behavior and to assert `usage()` mentions `--once`, `--linear-smoke`, `--pi-probe`, daemon default behavior, and the SIGINT/SIGTERM caveat.

21. In `src/scherzo/orchestrator/service.gleam`, keep the existing `Dependencies` and `run_once_with_dependencies` exports. Add production helpers `start_once`, `start_linear_smoke`, `start_daemon`, and `start_pi_probe`. Until daemon and pi-probe are implemented, placeholders may return explicit startup errors, but those placeholders must be removed in later milestones before final validation.

22. Add a structured production logger helper in service or a small logging module. Its signature must include level, event, fields, and current secrets. It should call `log.format(level, event, fields, secrets)` and write to stderr. Add a test that a secret configured through `LINEAR_API_KEY` is redacted from a startup log field.

23. Run `direnv exec . gleam run -- --help`. Expect usage to include all new modes and the daemon shutdown caveat. Run `direnv exec . gleam run -- --once examples/WORKFLOW.md` without `LINEAR_API_KEY`; expect a nonzero exit and a redacted `startup_failed` line with code `missing_tracker_api_key`, not a crash.

24. Create `src/scherzo/orchestrator/daemon.gleam`. Define `Message`, `WorkerCommand`, `WorkerHandle`, `TimerHandle`, `RuntimeDependencies`, and daemon state as described in Plan of Work and Interfaces.

25. Implement `daemon.start(workflow_path, dependencies)`. Startup must load and resolve the workflow, validate dispatch hooks, create `core.new_state`, build tracker and handoff clients, read and store the workflow file contents, schedule an immediate `PollTick(1)`, and return the actor subject. Do not scan all terminal Linear issues on startup.

26. Configure the actor selector to receive its subject messages and process monitor messages. Use `process.select_monitors` to map monitor down messages to `WorkerDown`.

27. Implement `daemon.shutdown(subject, timeout_ms)` with a reply subject. Shutdown handling must cancel the poll timer, cancel all retry timers, call `stop_worker` for all worker handles, clear worker handles, log `daemon_shutdown`, reply, and stop the actor.

28. Implement poll tick handling. It must read workflow contents and apply reload if changed, reconcile running issues by calling `tracker.fetch_issue_states_by_ids` for current running IDs, fetch candidates only when `config.can_dispatch(reload_state)` is true and `effective.agent.max_concurrent_agents` is not zero, sort candidates, dispatch eligible issues, and schedule the next poll using the current effective polling interval.

29. Implement retry tick handling. It must compare the message generation to the current `domain.RetryEntry.timer_generation`, log `retry_timer_stale` and do nothing for stale messages, defer without dispatch while the current workflow is invalid, refresh the issue by ID when valid, call `core.handle_retry_candidate`, and interpret the resulting effects.

30. Implement dispatch. Compute the deterministic workspace path, run handoff claim if enabled, call `core.apply_worker_start` with the computed path, generate a run ID, spawn an unlinked worker process, monitor it, store `WorkerHandle`, and wire runner `emit_update` to `WorkerUpdate`.

31. Implement `WorkerUpdate` handling. The daemon must log `pi_event` with issue ID, event name, and a truncated/redacted message. Do not drop the update silently. Add a comment in code that this is the Phase 2 EventHub seam.

32. Implement `WorkerFinished` handling. If no worker handle exists for the issue/run ID, log stale completion and ignore it. On success, call `core.apply_worker_success_with_workspace_path`; on failure, call `core.apply_worker_failure`. Apply handoff reports after core state transition. Interpret resulting effects and remove the worker handle.

33. Implement `WorkerDown` handling. If the monitor belongs to a known handle that has not already finished, remove the handle, log `worker_down`, turn it into a worker failure for scheduling purposes, and interpret resulting effects. If the monitor is stale, log `worker_down_stale` and ignore it.

34. Implement `apply_effects` for every `core.Effect`. For retry timers, store timer handles and cancel old timers on `CancelRetry`. For cleanup, call the cleanup dependency with workspace root, workspace path, and hooks. For `StopWorker`, call `stop_worker(handle)`.

35. Add `test/orchestrator_daemon_test.gleam`. Start with `daemon_poll_dispatches_fake_worker_routes_update_and_shutdown_test`. Use a temporary workflow, fake tracker, fake agent runner that emits one `PiUpdate` then returns success, and a logger that records lines. Manually send `PollTick` if using fake timers. Assert logs include `tick_started`, `dispatch_started`, `pi_event`, and `worker_exited`, then call `daemon.shutdown` and assert it returns.

36. Add `daemon_retry_timer_requeues_failed_worker_once_test`. Use a fake agent runner that fails on first call and succeeds on second call. Assert the second dispatch happens only after the matching `RetryTick`, stale generation messages do not start a third worker, and the self-claim does not block retry dispatch.

37. Add `daemon_invalid_reload_blocks_candidate_and_retry_dispatch_until_fixed_test`. Start with a valid paused workflow, rewrite it to invalid YAML with the same byte length if practical, send a poll, and assert `workflow_reload_failed` is logged and no dispatch occurs. Then rewrite valid unpaused content and assert a later poll dispatches. This proves content comparison catches fast edits.

38. Add `daemon_reconciles_terminal_issue_and_cleans_workspace_test`. Arrange a running worker that waits, have the fake tracker refresh the issue as Done, assert the cleanup function receives the deterministic workspace path, and assert the worker handle is removed through `StopWorker`.

39. Add `daemon_worker_crash_is_monitored_and_scheduled_for_retry_test`. Use a fake worker process or dependency that exits without sending `WorkerFinished`. Assert the daemon handles `WorkerDown`, removes the handle, logs `worker_down`, and schedules a retry or parks according to config.

40. Add `daemon_shutdown_cancels_retry_timers_and_stops_workers_test`. Start a daemon with one retry timer and one worker handle, call `daemon.shutdown`, and assert no later manual stale retry tick dispatches work.

41. Run `direnv exec . gleam test`. The daemon tests should fail until steps 25 through 34 are complete and pass afterward. The full run must end with all tests passing and no hangs.

42. Wire `service.start_daemon` to acquire the instance lock after config resolution and dispatch validation, start the daemon with production dependencies, and then block the main process with `process.sleep_forever()` after successful start. Document in code and help text that this CLI path does not run graceful `daemon.shutdown` on SIGINT/SIGTERM in Phase 1.

43. In `src/scherzo/error.gleam`, add `PiStallTimeout` to `PiRpcError` and map it to code `pi_stall_timeout`. Update exhaustive case expressions.

44. In `src/scherzo/agent/pi_rpc.gleam`, change active-turn reading to accept both `turn_timeout_ms` and `stall_timeout_ms`. Preserve strict `PiReadTimeout` for command-response reads. For prompt turns, repeated `port.ReadTimeout` before stall/turn deadlines should continue polling.

45. Change `pi_rpc.prompt` to accept `stall_timeout_ms` as well as `turn_timeout_ms`, or add `prompt_with_timeouts` and have `runner.run_attempt` call it. In `src/scherzo/agent/runner.gleam`, pass `config.pi.stall_timeout_ms`.

46. Update `test/fixtures/fake_pi_rpc.sh` if needed with modes for delayed events, no-output stall after prompt, and events without `agent_end`.

47. Update `test/pi_rpc_test.gleam` with `prompt_allows_short_read_timeouts_until_event_test`, `prompt_fails_when_stall_timeout_expires_test`, and `prompt_fails_when_turn_timeout_expires_before_agent_end_test`.

48. In `src/scherzo/agent/probe.gleam`, extend `probe` so it calls `pi_rpc.get_session_stats` after launch and before terminate. It must still not call `pi_rpc.prompt`. Update tests to assert the fake transcript contains `get_session_stats` during probe and does not contain `prompt`.

49. Implement `service.start_pi_probe(workflow_path)`. It should load and resolve the workflow, validate dispatch hooks because it prepares a workspace, acquire the instance lock, prepare a scratch workspace using a synthetic issue identifier such as `SCHERZO-PROBE`, call the extended probe with `config.pi.command`, run `workspace.cleanup` for the scratch workspace, release the lock on normal completion, and print or log `pi_probe_ok` without sending a prompt. If cleanup fails, log a warning but still return success if the probe itself passed.

50. Wire `--pi-probe` in `main.gleam` to `service.start_pi_probe`. Run `direnv exec . gleam run -- --pi-probe examples/WORKFLOW.md` without required env vars and expect a startup validation error, not a prompt. In tests, use a temporary workflow with fake pi and safe hooks and assert the transcript contains setup commands but no `prompt`.

51. Extend `src/scherzo/domain.gleam` with `HandoffConfig` and a `handoff` field in `EffectiveConfig`. The config should include `enabled`, `comment_on_claim`, `comment_on_success`, `comment_on_failure`, `claim_state_id`, `success_state_id`, and `failure_state_id`. Default `enabled` is `False`; comment booleans default to `True` when handoff is enabled; state IDs default to `None`.

52. In `src/scherzo/config.gleam`, parse top-level `handoff:`. Accept booleans for comment fields and non-empty strings for state IDs. Add config tests for default disabled handoff, enabled comments-only handoff, and state ID parsing. Update existing direct `EffectiveConfig` constructors.

53. In `src/scherzo/linear.gleam`, add GraphQL mutation builders `build_comment_create_request(config, issue_id, body)` and `build_issue_update_state_request(config, issue_id, state_id)`. Add parser logic that treats HTTP 200 GraphQL errors as `LinearGraphqlErrors` and any missing success payload as `LinearUnknownPayload`.

54. Add tests in `test/linear_test.gleam` asserting mutation requests include expected mutation names, variables, Authorization header, and never include the API key in error strings. Add response parsing tests for success, GraphQL error, and malformed success payload.

55. Create `src/scherzo/handoff.gleam`. Define a `Client` record with `claim_issue`, `report_success`, and `report_failure` functions. Add `linear_client(config, handoff_config, transport)` that performs configured comments and state updates through the new Linear mutation builders. Add `disabled_client()` that returns `Ok(Nil)` for all operations.

56. Add `test/handoff_test.gleam`. With a fake transport, assert comments-only handoff builds a claim comment containing the issue identifier and run ID, success comments include final classification and token totals, failure comments include only an error code or redacted message, and disabled handoff performs no transport calls.

57. Update daemon dispatch and completion paths to use handoff. A failed claim logs `handoff_claim_failed`, does not spawn a worker, and does not call `core.apply_worker_start`. Failed result reports log warnings and do not alter scheduling state.

58. Create `src/scherzo/instance_lock.gleam` and `src/scherzo_lock_ffi.erl`. Implement `acquire(root)` and `release(lock)` using the canonical absolute workspace root and exclusive creation of `.scherzo-state/instance.lock`.

59. Add `test/instance_lock_test.gleam`. Acquire a lock under a temporary workspace root, assert a second acquire returns an error, release the first lock, and assert a third acquire succeeds. Also assert equivalent relative and absolute roots conflict, while different roots do not conflict.

60. In `service.start_daemon`, `service.start_once`, and `service.start_pi_probe` with production dependencies, acquire the instance lock after resolving config and before any work that can dispatch or prepare a workspace. Release it when one-shot execution or pi-probe completes normally. `--linear-smoke` must not acquire the lock because it is read-only. Daemon CLI lock release on SIGINT/SIGTERM is not guaranteed in Phase 1; document stale lock recovery.

61. Update `README.md`. Document `--once`, `--linear-smoke`, `--pi-probe`, daemon default behavior, the programmatic-vs-CLI shutdown distinction, local instance lock behavior, stale lock recovery, handoff config, comments-only mode, optional state IDs, and the continued prohibition on multiple hosts or multiple workspace roots for the same Linear project.

62. Update `examples/WORKFLOW.md` with commented or sample `handoff:` settings that are safe by default. Keep `agent.max_concurrent_agents` conservative. Include comments explaining that state IDs are Linear IDs, not state names.

63. Run final deterministic validation from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -- --help

The test command should end with all tests passing and no failures. The exact pass count will be higher than the current baseline 81; record the final count in Progress.

64. With a private Linear test project and no dispatch, run:

    LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --linear-smoke path/to/WORKFLOW.md

Expect a line containing `linear_smoke_ok` with candidate, terminal, and refreshed counts, no more than the documented bounded sample requests, and no `dispatch_started` or `prompt` logs.

65. With a safe workflow whose `pi.command` runs the repository fixture `test/fixtures/fake_pi_rpc.sh` and whose hooks create a harmless workspace marker, run daemon mode against one private test issue. Expect repeated `tick_started` logs, one `dispatch_started`, `pi_event` logs for fake-pi events, fake-pi transcript entries for `get_state`, `get_session_stats`, and `prompt`, and handoff comments in Linear only if `handoff.enabled: true`.

66. After smoke, probe, fake-pi real-Linear dispatch, and handoff behavior are all verified, run `--pi-probe` with the real pi RPC command in a scratch workflow. Expect `pi_probe_ok` and no prompt transcript. Only after that, try one safe real pi issue in a disposable repository workspace.

## Testing and Falsifiability

Every new production path must have deterministic tests that do not require real Linear or real pi. The real HTTP transport is tested through pure request conversion, client delegation with fake transports, and error mapping. The bounded read-only smoke path is tested with fake readers for non-empty, empty, and no-extra-pagination cases. The CLI parser is tested for every new mode and for invalid flags.

The core fix is falsified if a self-claimed issue with a valid retry entry cannot emit `Dispatch` from `handle_retry_candidate`, or if normal candidate dispatch starts allowing arbitrary already-claimed issues. Both cases need explicit tests.

The daemon is falsified if any of these scenarios fail: it dispatches while `agent.max_concurrent_agents` is zero; it dispatches candidates or retries while the current workflow reload is invalid; it starts two workers for the same issue from a stale retry generation; it loses the claim when retry polling fails; it fails to cleanup a terminal running issue; it crashes when the logger returns `Error(Nil)`; it drops `WorkerUpdate` without logging; a worker crash leaves a stale handle; or `daemon.shutdown` leaves retry timers able to dispatch later. Add explicit tests for each behavior in `test/orchestrator_daemon_test.gleam`.

The pi timeout changes are falsified if `pi.read_timeout_ms` still acts as a hard turn failure for an otherwise healthy delayed event, if `pi.turn_timeout_ms` can be exceeded without `PiTurnTimeout`, if no-output hangs fail with the wrong error, or if command-response reads stop using `PiReadTimeout` for short required responses.

The handoff path is falsified if an API key appears in a comment, log, error message, or test failure output; if disabled handoff sends any Linear mutation; if claim failure still spawns a worker; if comments omit run ID; or if report failure crashes the daemon after a worker has already completed.

The local instance lock is falsified if two acquisitions for the same canonical workspace root can both succeed, if equivalent relative and absolute paths do not conflict, if a lock under one root blocks another root, or if a stale lock error message implies distributed safety.

Credential-gated validation is intentionally separate from deterministic tests. `--linear-smoke` proves real Linear read compatibility and should be run before daemon mode. `--pi-probe` proves real pi command compatibility without a prompt and should be run before any real issue dispatch. If either smoke or probe fails, do not proceed to real dispatch; update this plan's Surprises & Discoveries and fix the mismatch first.

## Validation and Acceptance

The deterministic acceptance gate is:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -- --help

Accept the gate only if format exits zero, tests end with all tests passed and no failures, and help text documents daemon mode plus `--once`, `--linear-smoke`, `--pi-probe`, the one-instance limitation, and the Phase 1 CLI shutdown caveat.

The read-only Linear acceptance gate is:

    LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --linear-smoke path/to/WORKFLOW.md

Accept the gate only if it logs `linear_smoke_ok`, prints counts, returns zero, uses the documented bounded sample reads, does not require dispatch hooks, and does not log `dispatch_started`, `worker_exited`, or any pi event. If the workflow has no matching issues, zero counts are acceptable.

The real-pi no-prompt acceptance gate is:

    LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- --pi-probe path/to/WORKFLOW.md

Accept the gate only if it logs `pi_probe_ok`, returns zero, and any available transcript or pi diagnostics show no `prompt` command. It may create and cleanup a scratch workspace.

The daemon acceptance gate for fake-pi real-Linear dispatch is to run a private test workflow with one harmless active issue, `agent.max_concurrent_agents: 1`, a fake `pi.command`, and safe hooks. Accept only if exactly one worker starts for the issue, every fake-pi event appears as a daemon `pi_event` log, retry behavior is bounded if the fake worker fails, terminal issue cleanup happens when the issue reaches a terminal state, and programmatic `daemon.shutdown` in tests cancels timers and stops live workers. This gate does not claim shell Ctrl-C runs graceful cleanup in Phase 1.

The handoff acceptance gate is to enable comments-only handoff in the private test workflow and observe a claim comment and one result comment in Linear, both containing the daemon run ID. If optional state IDs are configured, accept only if the issue moves to the configured states and the daemon logs mutation success without exposing the API key.

## Rollout, Recovery, and Idempotence

Roll out in stages. First merge the deterministic code with default handoff disabled. Then run `--linear-smoke` with a workflow that has no safe candidates or has `agent.max_concurrent_agents: 0`; smoke is read-only and does not use the agent setting but this keeps the same workflow safe if reused. Then run `--pi-probe` with a scratch workspace. Then run daemon mode with fake pi against one private Linear test issue. Only after those stages pass should real pi run against one safe issue in a disposable repository.

If startup fails with an instance-lock error, do not delete the lock blindly. Check whether another Scherzo process is running with the same canonical workspace root. If no process exists and the previous process crashed or was terminated by the shell, remove `workspace.root/.scherzo-state/instance.lock` manually and restart.

If a workflow reload becomes invalid, the daemon keeps running workers and reconciliation but blocks new candidate and retry dispatch. Fix `WORKFLOW.md`; on the next valid content change, dispatch resumes. If the reload changed the API key, the daemon must register the new secret before logging reload summaries.

If handoff comments or state mutations fail, the daemon logs the failure and keeps scheduling state safe. A failed claim prevents dispatch for that tick; a failed result report does not rerun the worker. Operators can manually add a Linear comment or state transition from logs using the run ID, issue ID, and mutation kind.

If a worker is killed during programmatic shutdown or reconciliation, workspaces are preserved unless a terminal cleanup effect applies. This keeps partial work available for inspection. Operators may delete workspaces manually after the daemon stops.

## Artifacts and Notes

Current validation captured after implementation:

    direnv exec . gleam format --check src test
    # exited 0

    direnv exec . gleam test
    101 passed, no failures

    direnv exec . gleam run -- --help
    Usage: gleam run -- [mode] [path-to-WORKFLOW.md]
    Scherzo polls Linear and runs pi agents in per-issue workspaces. With no mode, Scherzo runs daemon mode and keeps polling until the VM process is terminated.

    direnv exec . gleam run -- --linear-smoke examples/WORKFLOW.md
    level=info service=scherzo event=linear_smoke_ok candidate_count=0 terminal_count=0 refreshed_count=0

    direnv exec . gleam run -- --linear-smoke test/tmp/real-linear-validation/WORKFLOW.md
    level=info service=scherzo event=linear_smoke_ok candidate_count=1 terminal_count=0 refreshed_count=1

    direnv exec . gleam run -- --pi-probe test/tmp/real-linear-validation/WORKFLOW.md
    level=info service=scherzo event=pi_probe_ok workspace_path=.../test/tmp/real-linear-validation/workspaces/SCHERZO-PROBE

    direnv exec . gleam run -- --pi-probe test/tmp/real-linear-validation/WORKFLOW.real-pi.md
    level=info service=scherzo event=pi_probe_ok workspace_path=.../test/tmp/real-linear-validation/workspaces/SCHERZO-PROBE

    fake-pi daemon validation events with temporary Backlog setup:
    tick_started, candidates_fetched, dispatch_started, pi_event, pi_event, pi_event, pi_event, pi_event, worker_exited, issue_parked, claim_released

    direnv exec . gleam run -- --linear-smoke test/tmp/real-linear-validation/WORKFLOW.todo-fake-pi.md
    level=info service=scherzo event=linear_smoke_ok candidate_count=1 terminal_count=0 refreshed_count=1

    fake-pi daemon validation events for LIV-7 in Todo:
    tick_started, candidates_fetched, dispatch_started, pi_event, pi_event, pi_event, pi_event, worker_exited, issue_parked, claim_released

    fake-pi LIV-7 transcript commands:
    set_session_name,set_auto_retry,get_state,get_session_stats,set_session_name,set_auto_retry,get_state,prompt,get_session_stats

    comments-only handoff for LIV-7:
    before_comment_count 0
    after_comment_count 2
    state Todo
    Scherzo claimed LIV-7 for run LIV-7--576460751605.
    Scherzo completed run LIV-7--576460751605 for LIV-7 with classification active and 3 total pi tokens.

    state-transition handoff for LIV-7:
    before_state Todo
    before_comment_count 2
    after_state Done
    after_comment_count 4
    Scherzo claimed LIV-7 for run LIV-7--576460751376.
    Scherzo completed run LIV-7--576460751376 for LIV-7 with classification non_active and 3 total pi tokens.

    first real-pi handoff attempt for LIV-8:
    dispatch_started issue_identifier=LIV-8 run_id=LIV-8--576460751551
    worker_stop_requested issue_id=95c885ca-6825-4502-90f2-1b5b3a61f634 reason=non_active
    state In Progress
    comment_count 1
    Scherzo claimed LIV-8 for run LIV-8--576460751551.

    successful real-pi validation for LIV-8:
    daemon events: tick_started, candidates_fetched, dispatch_started, pi_event, pi_event, pi_event, pi_event, pi_event, pi_event, pi_event, pi_event, worker_exited, issue_parked, claim_released
    after_state Done
    after_comment_count 3
    Scherzo claimed LIV-8 for run LIV-8--576460751523.
    Scherzo completed run LIV-8--576460751523 for LIV-8 with classification active and 7769 total pi tokens.

    clean real-pi validation for LIV-9:
    moved setup state: Backlog -> Todo
    smoke: linear_smoke_ok candidate_count=1 terminal_count=2 refreshed_count=1
    daemon events: tick_started, candidates_fetched, dispatch_started, pi_event, pi_event, pi_event, pi_event, pi_event, pi_event, pi_event, pi_event, worker_exited, issue_parked, claim_released
    after_state Done
    after_comment_count 2
    Scherzo claimed LIV-9 for run LIV-9--576460751521.
    Scherzo completed run LIV-9--576460751521 for LIV-9 with classification active and 7767 total pi tokens.

Important current code facts to normalize during implementation:

    src/scherzo/orchestrator/service.gleam currently contains private real Linear HTTP transport helpers.
    src/scherzo/orchestrator/service.gleam currently passes "" as the workspace path to core.apply_worker_start.
    src/scherzo/orchestrator/core.gleam currently blocks retry dispatch on the issue's own claim.
    src/scherzo/agent/pi_rpc.gleam currently has a turn deadline but treats read timeout as a hard active-turn error and does not enforce stall_timeout_ms.
    src/scherzo/config.gleam currently validates dispatch hooks inside resolve_hooks.
    src/scherzo/config.gleam already has ReloadState and apply_reload, but no live loop uses them.

When recording future validation output in this plan, keep transcripts short and redact project names, issue titles, repository URLs, and API keys if they are sensitive.

## Interfaces and Dependencies

In `src/scherzo/orchestrator/core.gleam`, preserve normal dispatch behavior and add retry-aware handling inside `handle_retry_candidate`. The public API may stay the same, but tests must prove this transition:

    apply_worker_failure(started_state, config, issue.id, now)
    |> handle_retry_candidate(config, issue.id, Ok(Some(issue)))
    // effects include Dispatch(issue), not another retry solely due to state.claimed

In `src/scherzo/linear.gleam`, define or preserve these public interfaces:

    pub type Transport = fn(Request) -> Result(Response, error.TrackerError)

    pub fn client(
      config: domain.TrackerConfig,
      transport: Transport,
    ) -> tracker.Client

    pub fn real_client(config: domain.TrackerConfig) -> tracker.Client

    pub fn http_transport(request: Request) -> Result(Response, error.TrackerError)

    pub fn http_error_to_string(error: httpc.HttpError) -> String

    pub fn build_comment_create_request(
      config: domain.TrackerConfig,
      issue_id: String,
      body: String,
    ) -> Result(Request, error.TrackerError)

    pub fn build_issue_update_state_request(
      config: domain.TrackerConfig,
      issue_id: String,
      state_id: String,
    ) -> Result(Request, error.TrackerError)

In `src/scherzo/smoke.gleam`, define:

    pub type LinearSmokeReader {
      LinearSmokeReader(
        fetch_candidate_sample: fn() -> Result(List(domain.Issue), error.TrackerError),
        fetch_terminal_sample: fn(List(String)) -> Result(List(domain.Issue), error.TrackerError),
        refresh_issue_states_by_ids: fn(List(String)) -> Result(List(domain.Issue), error.TrackerError),
      )
    }

    pub type LinearSmokeResult {
      LinearSmokeResult(
        candidate_count: Int,
        terminal_count: Int,
        refreshed_count: Int,
      )
    }

    pub fn linear_read_smoke(
      reader: LinearSmokeReader,
      terminal_states: List(String),
    ) -> Result(LinearSmokeResult, error.TrackerError)

In `src/scherzo/domain.gleam`, add:

    pub type HandoffConfig {
      HandoffConfig(
        enabled: Bool,
        comment_on_claim: Bool,
        comment_on_success: Bool,
        comment_on_failure: Bool,
        claim_state_id: Option(String),
        success_state_id: Option(String),
        failure_state_id: Option(String),
      )
    }

and add `handoff: HandoffConfig` to `EffectiveConfig`.

In `src/scherzo/handoff.gleam`, define:

    pub type Client {
      Client(
        claim_issue: fn(domain.Issue, String) -> Result(Nil, error.TrackerError),
        report_success: fn(domain.Issue, runner.WorkerSuccess, String) -> Result(Nil, error.TrackerError),
        report_failure: fn(domain.Issue, runner.WorkerFailure, String) -> Result(Nil, error.TrackerError),
      )
    }

    pub fn disabled_client() -> Client

    pub fn linear_client(
      tracker_config: domain.TrackerConfig,
      handoff_config: domain.HandoffConfig,
      transport: linear.Transport,
    ) -> Client

The `String` argument in handoff methods is a per-run ID generated by the daemon. It must be stable for one worker attempt and included in comments so operators can correlate logs with Linear comments.

In `src/scherzo/orchestrator/daemon.gleam`, define:

    pub type WorkerCommand {
      Abort
      StopAfterCurrentTurn
      QueuePrompt(String)
      RespondToUi(String, String)
    }

    pub type Message {
      PollTick(Int)
      RetryTick(String, Int)
      WorkerFinished(String, String, Result(runner.WorkerSuccess, runner.WorkerFailure))
      WorkerUpdate(String, runner.PiUpdate)
      WorkerDown(process.Down)
      Shutdown(process.Subject(Nil))
      GetSnapshot(process.Subject(domain.RuntimeState))
    }

    pub type WorkerHandle {
      WorkerHandle(
        issue_id: String,
        run_id: String,
        pid: process.Pid,
        monitor: process.Monitor,
        workspace_path: String,
        command_subject: Option(process.Subject(WorkerCommand)),
      )
    }

    pub type TimerHandle {
      RealTimer(process.Timer)
      TestTimer(Int)
    }

    pub type RuntimeDependencies {
      RuntimeDependencies(
        make_tracker: fn(domain.TrackerConfig) -> tracker.Client,
        make_handoff: fn(domain.TrackerConfig, domain.HandoffConfig) -> handoff.Client,
        agent_runner: fn(
          domain.Issue,
          Option(Int),
          domain.WorkflowDefinition,
          domain.EffectiveConfig,
          tracker.Client,
          fn(String, runner.PiUpdate) -> Nil,
        ) -> Result(runner.WorkerSuccess, runner.WorkerFailure),
        cleanup: fn(String, String, domain.HooksConfig) -> Result(Nil, error.WorkspaceError),
        logger: fn(String, String, List(log.Field), List(String)) -> Result(Nil, Nil),
        now_ms: fn() -> Int,
        send_after: fn(process.Subject(Message), Int, Message) -> TimerHandle,
        cancel_timer: fn(TimerHandle) -> Nil,
      )
    }

    pub fn start(
      workflow_path: Option(String),
      dependencies: RuntimeDependencies,
    ) -> Result(actor.Started(process.Subject(Message)), service.StartupError)

    pub fn shutdown(
      subject: process.Subject(Message),
      timeout_ms: Int,
    ) -> Result(Nil, Nil)

In `src/scherzo/instance_lock.gleam`, define:

    pub opaque type Lock

    pub type LockError {
      LockAlreadyHeld(String)
      LockIo(String)
    }

    pub fn acquire(workspace_root: String) -> Result(Lock, LockError)

    pub fn release(lock: Lock) -> Nil

No new runtime package dependency beyond the current `gleam.toml` dependencies should be necessary. The lock and any optional process-tree test helpers should use small Erlang FFI modules in `src/` and deterministic tests under `test/`.
