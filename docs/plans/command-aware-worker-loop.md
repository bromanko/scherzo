# Add command-aware worker loop for prompts and operator UI

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, live Scherzo workers can receive operator commands without corrupting pi RPC command ordering. An operator can abort a session gracefully, ask a session to stop after the current turn, queue a follow-up prompt for the next turn, and answer a blocking pi extension UI request when the workflow explicitly sets `pi.ui_request_policy: operator`. The visible proof is that `scripts/scherzoctl prompt <session-id> "summarize progress"` sent during a turn is queued and used as the next pi prompt, `scripts/scherzoctl ui respond <session-id> ui-1 --cancel` releases an operator-managed UI request, and `scripts/scherzoctl abort <session-id> --yes` sends pi's `abort` RPC command before the worker exits and the daemon parks the issue.

This plan completes the worker-owned half of the mutating operator controls work. The local control protocol, `scherzoctl` parsing, token authentication, daemon command routing, and command result encoding already exist. What remains is to make the worker that owns the pi process also own command delivery, prompt queueing, and pending UI state.

## Problem Framing and Constraints

The current daemon can authenticate and route mutating commands, but spawned workers still store `command_subject: None`. Because `src/scherzo/agent/pi_rpc.gleam` owns the full prompt read loop, a live worker cannot currently observe `QueuePrompt` or `RespondUi` while pi is streaming a turn. The daemon fallback for `abort` and `stop-after-current-turn` kills the worker process and parks the issue, which is safe, but it does not send pi's graceful `abort` command and cannot implement true stop-after-turn semantics.

The hard constraint is single ownership of pi RPC stdin and command ids. The control server must not write to pi. The daemon may route commands by session id, but the worker process must be the only process that sends pi RPC messages such as `prompt`, `abort`, `get_session_stats`, and `extension_ui_response`. Prompt injection in the middle of a streaming turn is not safe, so prompts must be queued and sent only when the worker is between turns.

The second constraint is liveness. An operator UI policy must not hang a worker forever. A pending UI request needs a timeout, and the existing `cancel`, `fail`, and `ignore` policies must keep their behavior after the worker loop is refactored.

## Strategy Overview

Add a small worker-command module and a command-aware runner loop. The daemon will create a command subject for every spawned worker, pass that subject into the runner, and store the subject in `WorkerHandle.command_subject`. The new module defines both `worker_command.Command`, which the daemon sends to the worker, and `worker_command.Reply`, which the worker sends back. The worker never constructs `control/command.CommandResult`; the daemon keeps the original `OperatorCommand`, waits for the worker reply with the local control timeout, and converts the agent-level reply into the public command result so the command name and target session id stay correct.

Refactor `src/scherzo/orchestrator/daemon.gleam` so `ApplyOperatorCommand` carries the command timeout in milliseconds. For prompt, UI response, and stop-after-current-turn commands, the daemon sends the worker command and waits for the worker reply. If the worker does not reply before the timeout, those commands return `rejected` with reason `worker_command_timeout`. Abort is the only command with a destructive safety fallback: if the worker command subject is absent or the worker does not acknowledge before the timeout, the daemon uses the existing kill-and-park path and returns `applied` with reason `operator_abort`.

Refactor `src/scherzo/agent/pi_rpc.gleam` to expose stepwise helpers that preserve `pi_rpc.Session.next_id`. The current high-level `prompt` and `prompt_with_ui_policy` functions should remain for compatibility tests, but the daemon worker path should use helpers such as `send_prompt`, `read_turn_record`, `send_abort`, `send_extension_ui_cancel`, and `send_extension_ui_value`. Helpers that wait for a command response must not silently discard pi events that arrive before the response; they return any skipped `RpcRecord`s to the caller, and the runner emits those records before continuing. Each helper keeps pi command/response correlation inside `pi_rpc` and does not expose raw ports outside the agent layer.

Implement command polling inside `src/scherzo/agent/runner.gleam` rather than in the daemon. Add a command-aware variant of `run_attempt` that receives a worker command subject. The existing `run_attempt` remains as a synchronous compatibility wrapper that creates an unused command subject and calls the new command-aware function. This avoids rewriting all non-daemon runner tests at once while allowing `RuntimeDependencies.agent_runner` in `src/scherzo/orchestrator/daemon.gleam` to pass a real subject.

Represent operator abort and stop-after-turn as explicit runner outcomes, not ordinary pi failures. Add `OperatorAbort` and `OperatorStopAfterCurrentTurn` to `error.AgentRunnerError`, update `error.agent_code`, and extend `runner.WorkerFailure` so it carries the token totals accumulated before exit and an optional refreshed final issue. Teach daemon `finish_worker_failure` to use that metadata for operator exits, finish the session with `operator_abort` or `operator_stop_after_current_turn`, cancel retry timers, release running state, park the issue, skip ordinary retry scheduling, and skip ordinary `ReportFailure` handoff side effects.

Keep prompt queueing intentionally small. The worker owns a FIFO prompt queue with a hard cap of 10 messages. Because the daemon waits for each worker acknowledgement, rejected prompts are not reported as accepted and a burst of concurrent local control calls backs up at the daemon timeout boundary rather than becoming an unbounded worker-owned prompt queue. A prompt command received during a turn returns `queued`. A prompt command received between turns either becomes the next prompt immediately and returns `applied`, or is queued if another operator prompt is already waiting. If the queue is full, return `rejected` with reason `prompt_queue_full`. If the session exits before queued prompts are sent, emit one `operator_prompt_dropped` update per dropped prompt, with redacted/truncated content.

Implement operator UI as a state in the worker loop. Under `pi.ui_request_policy: operator`, a blocking UI request creates one pending request containing the request id, method, decoded message excerpt, created time, and absolute UI deadline. While pending, the worker accepts `RespondUi` for that exact request id. It sends cancel or value through `pi_rpc`, publishes response events, and resumes the turn read loop. While waiting for the operator, the loop still respects the overall turn deadline, but it pauses the ordinary stall timeout because pi is intentionally blocked on operator input. The runner passes the UI deadline as the temporary activity deadline to `read_turn_record`, so the next read wait is bounded by the minimum of `pi.read_timeout_ms`, the remaining UI timeout, and the remaining turn timeout. If that temporary activity deadline expires while UI is pending, the runner treats it as UI timeout rather than ordinary `PiStallTimeout`, sends cancel, publishes `operator_ui_timeout`, resets the real stall deadline, and resumes. Under `cancel`, `fail`, and `ignore`, keep the behavior covered by the existing `test/pi_rpc_test.gleam` tests.

## Alternatives Considered

One alternative is to keep the daemon fallback for abort and implement prompts and UI responses by writing to the pi port from the daemon. That is rejected because it creates two writers for one JSON Lines RPC stream and can break pi command/response correlation.

Another alternative is to keep `pi_rpc.prompt_with_ui_policy` as the only turn loop and check worker commands only before and after each turn. That is rejected because abort would wait until a potentially long turn finishes, UI responses could not be delivered while pi is waiting, and queued prompts could not be acknowledged deterministically during a turn.

A third alternative is to spawn a separate worker-command process that writes to pi while the existing runner reads. That is also rejected because the writer would still be outside the process that owns `pi_rpc.Session.next_id`, and it would not know which pi request id is pending.

The chosen approach is larger than the daemon fallback, but it is the smallest design that preserves single-owner pi RPC ordering while enabling prompt and UI commands.

## Risks and Countermeasures

The main protocol risk is corrupting pi command ids or losing interleaved pi events. Countermeasure: all pi writes go through `src/scherzo/agent/pi_rpc.gleam` helpers that take and return `pi_rpc.Session`. The daemon and control server never see the raw port and never increment `next_id` themselves. Response-waiting helpers return skipped event records instead of dropping them. Tests inspect `test/fixtures/fake_pi_rpc.sh` transcripts to prove `prompt`, `abort`, `get_session_stats`, and `extension_ui_response` appear in a valid order, and add an interleaving mode where an event arrives before a command response.

The main command-result risk is returning a worker-built result with the wrong command name or target. Countermeasure: the worker replies only with `worker_command.Reply`; the daemon converts that reply to `control/command.CommandResult` using the original `OperatorCommand` that came from the control API.

The main scheduler risk is treating operator abort or stop-after-current-turn like a normal failure and redispatching a runaway issue. Countermeasure: add explicit `OperatorAbort` and `OperatorStopAfterCurrentTurn` runner failure reasons and daemon handling that finishes the session with the operator reason, updates any already-known token totals, cancels retry timers, releases running state, and parks the issue without enqueuing ordinary `ReportFailure`.

The main liveness risk is an operator UI request hanging forever or timing out much later than configured. Countermeasure: `pi.ui_request_timeout_ms` is already parsed and defaults to 300000 ms. The worker stores an absolute UI deadline, bounds each pending-UI read by that deadline, sends a cancel response on timeout, and never lets the ordinary stall timeout fire while pi is waiting for operator input. Tests use tiny UI timeouts shorter than `pi.read_timeout_ms` to prove there is no hang or delayed cancellation.

The main UX risk is operators misunderstanding whether a prompt interrupted the current turn. Countermeasure: command results use `queued` when a turn is active and `applied` only when a command takes effect immediately. Session events include `operator_prompt_queued`, `operator_prompt_sent`, and `operator_prompt_dropped`.

The main compatibility risk is breaking existing synchronous runner and pi RPC tests. Countermeasure: preserve `runner.run_attempt`, `pi_rpc.prompt`, and `pi_rpc.prompt_with_ui_policy` as compatibility wrappers. Add new command-aware helpers rather than deleting old APIs in the first milestone.

The main test stability risk is race conditions around command delivery. Countermeasure: tests use fake pi modes that delay a turn, interleave command responses with pi events, or block on UI in deterministic ways, and command replies are received through explicit reply subjects before assertions continue.

## Progress

- [x] (2026-04-28 01:12Z) Drafted this plan from the current tree after `direnv exec . gleam format --check src test` and `direnv exec . gleam test` passed with 154 tests.
- [x] (2026-04-28) Revised the plan after adversarial review to close worker reply ownership, daemon command timeout, UI deadline, skipped pi event, and operator-exit metadata gaps.
- [x] (2026-04-28 17:05Z) Added `src/scherzo/agent/worker_command.gleam`, registered worker-owned command subjects with the daemon, and made daemon routing wait for worker replies with timeout conversion.
- [x] (2026-04-28 17:05Z) Added stepwise pi RPC helpers and `RpcRecord.message` while preserving the high-level pi RPC API.
- [x] (2026-04-28 17:05Z) Implemented the command-aware runner loop with worker-level abort and stop-after-current-turn outcomes.
- [x] (2026-04-28 17:05Z) Implemented FIFO operator prompt queueing, queue cap enforcement, sent events, and dropped-prompt auditing.
- [x] (2026-04-28 17:05Z) Implemented operator-managed UI wait, response routing, and timeout cancellation in the runner.
- [x] (2026-04-28 17:05Z) Updated daemon operator-exit handling, README documentation, related plan notes, and final validation. `direnv exec . gleam test` passed with 175 tests.

## Surprises & Discoveries

- Observation: The current daemon has command routing but no live worker command subjects.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` defines `WorkerCommand` variants with reply subjects and routes `PromptSession` and `RespondUi` through `route_worker_command`, but `spawn_worker` still creates `WorkerHandle(... command_subject: None)`.

- Observation: The current pi RPC loop already distinguishes `cancel`, `fail`, `ignore`, and `operator`, but `operator` is not implemented in the current tree.
  Evidence: `src/scherzo/agent/pi_rpc.gleam` has `prompt_with_ui_policy` and `handle_blocking_ui_request`; the `domain.Operator` branch returns `error.PiProtocolError("operator UI policy is not implemented")`.

- Observation: The current fake pi fixture emits a UI request and then continues without waiting for `extension_ui_response`.
  Evidence: `test/fixtures/fake_pi_rpc.sh` writes the `extension_ui_request` JSON inside the `prompt)` branch and then proceeds to `turn_end` unless another stall variable is set. Operator UI tests need a fixture mode that waits for a response before continuing.

- Observation: Gleam process subjects are receive-owned by the process that creates them.
  Evidence: An early implementation created the worker command subject in the daemon and passed it into the spawned worker; `runner` crashed with `Cannot receive with a subject owned by another process`. The final implementation creates the command subject inside the worker process and registers it with the daemon using `WorkerCommandReady`.

- Observation: Manual UI smoke testing found that workflow parsing still rejected `pi.ui_request_policy: operator` even though the runner supported it.
  Evidence: Starting a temporary daemon with a workflow containing `ui_request_policy: operator` failed with `StartupError("invalid_config", "config error")`. `src/scherzo/config.gleam` still returned `InvalidConfig("pi.ui_request_policy operator is not supported yet")`. The parser now returns `domain.Operator`, and `test/config_test.gleam` covers it.

- Observation: The baseline test count in the current tree had already moved from the plan's 154 tests to 164 tests before implementation began.
  Evidence: The first validation run for this implementation returned `164 passed, no failures`.

## Decision Log

- Decision: Define worker command messages and worker replies in a new agent-level module rather than in `src/scherzo/orchestrator/daemon.gleam`.
  Rationale: Worker commands are consumed by the runner and merely routed by the daemon. Keeping the command and reply types in the agent layer avoids making the runner import the daemon module, while keeping public `CommandResult` construction in the daemon where the original `OperatorCommand` and target session id are available.
  Date: 2026-04-28

- Decision: Keep `runner.run_attempt` as a compatibility wrapper and add a command-aware function instead of replacing every caller directly.
  Rationale: Many tests and non-daemon call sites use the synchronous runner shape. A wrapper keeps the migration smaller and makes the daemon-specific command path explicit.
  Date: 2026-04-28

- Decision: Add explicit operator runner failure reasons and failure metadata.
  Rationale: Operator safety stops must park the issue and avoid retry bookkeeping. Reusing ordinary `PiFailed` would schedule retries and hide the operational intent. Carrying accumulated tokens and an optional refreshed final issue lets the daemon preserve the information known at a graceful stop point.
  Date: 2026-04-28

- Decision: Use daemon-side worker command timeouts, with destructive fallback only for abort.
  Rationale: Prompt, UI response, and stop-after-current-turn commands should not claim success if the worker did not acknowledge them. Abort is different because it is an operator safety action; if graceful abort is unavailable, the existing kill-and-park behavior is safer than leaving the worker running.
  Date: 2026-04-28

- Decision: Pause ordinary stall timeout while an operator UI request is pending.
  Rationale: During operator UI, pi is intentionally waiting for human input. The correct liveness boundary is the UI timeout plus the overall turn timeout, not the ordinary no-output stall timeout.
  Date: 2026-04-28

- Decision: Use a fixed prompt queue cap of 10.
  Rationale: It is large enough for realistic operator follow-up bursts and small enough to avoid unbounded memory or confusing delayed instructions.
  Date: 2026-04-28

- Decision: Let the worker process create the command subject and register it with the daemon, rather than creating the subject in the daemon before spawning.
  Rationale: Gleam subjects can only be received by their owner process. Creating the subject in the worker keeps command polling legal and still lets the daemon route by session id after it receives `WorkerCommandReady` for the matching issue and run id.
  Date: 2026-04-28

- Decision: Use a slightly shorter worker-reply wait than the public command timeout inside the daemon.
  Rationale: `apply_operator_command` waits for the daemon reply using the same local control timeout. If the daemon waits the full timeout for a worker reply, the caller can time out before the daemon sends the converted `worker_command_timeout` result. Subtracting a small margin lets the daemon return a command-level timeout deterministically.
  Date: 2026-04-28

## Outcomes & Retrospective

Implementation completed on 2026-04-28. Live daemon workers now expose worker-owned command subjects, and daemon command routing converts `worker_command.Reply` into public `CommandResult` values. Prompt commands can reach live workers, prompts sent during a turn are queued for the next prompt with a cap of 10, and operator UI requests under `pi.ui_request_policy: operator` can be configured, answered through `RespondUi`, or cancelled by timeout. Operator abort and stop-after-current-turn use explicit runner failure reasons so the daemon parks the issue without ordinary retry or failure handoff side effects.

One important implementation adjustment was required: Gleam process subjects can only be received by the process that created them. The worker process now creates its command subject and immediately registers it with the daemon through `WorkerCommandReady`; the daemon stores it on `WorkerHandle` after validating the run id. This preserves the plan's single-owner command behavior while respecting the runtime's subject ownership rule.

Final validation from the repository root passed:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    175 passed, no failures

## Context and Orientation

Scherzo is a Gleam/Erlang daemon. `src/scherzo/orchestrator/daemon.gleam` owns scheduler state, claims, retry timers, worker handles, and the local control server. `src/scherzo/control/command.gleam` defines transport-independent operator commands and command results. `src/scherzo/control/protocol.gleam`, `src/scherzo/control/server.gleam`, `src/scherzo/control/client.gleam`, and `src/scherzo/ctl.gleam` already map local `scherzoctl` requests to those commands.

`src/scherzo/agent/runner.gleam` prepares a workspace, renders the issue prompt, optionally runs a pi compatibility probe, launches pi RPC, sends prompts, reads pi events until `agent_end`, fetches issue state after each turn, and returns `WorkerSuccess` or `WorkerFailure`. `src/scherzo/agent/pi_rpc.gleam` wraps the JSON Lines pi RPC protocol and currently owns high-level `prompt` loops.

A pi RPC session is represented by `pi_rpc.Session(process, command, cwd, session_id, next_id)`. `next_id` is the next command id for ordinary pi commands. The same process that sends commands must update this value so future command/response matching remains correct.

A worker command subject is a Gleam process subject stored in `daemon.WorkerHandle.command_subject`. The daemon uses it to send worker-level commands by session id and waits on a per-command reply subject. The worker reply is an agent-level `worker_command.Reply`; the daemon converts it to `control/command.CommandResult` using the original operator command. In the implemented design, the spawned worker process creates the subject that it will receive from, sends `WorkerCommandReady(issue_id, run_id, subject)` to the daemon, and the daemon stores `Some(subject)` on the matching `WorkerHandle`.

## Preconditions and Verified Facts

This plan assumes the mutating control slice exists in the current tree:

- `src/scherzo/control/command.gleam` defines `OperatorCommand`, `UiResponse`, `CommandStatus`, and `CommandResult`.
- `src/scherzo/orchestrator/daemon.gleam` has `ApplyOperatorCommand` and routes `PromptSession` and `RespondUi` through `route_worker_command` if a worker handle has a command subject, but `ApplyOperatorCommand` does not yet carry the local control timeout and `route_worker_command` currently discards the worker reply subject and immediately returns `queued`.
- `src/scherzo/orchestrator/daemon.gleam` still defines `pub type WorkerCommand` locally. This plan replaces that type with an agent-level command type and a separate agent-level reply type.
- `spawn_worker` in `src/scherzo/orchestrator/daemon.gleam` still calls `dependencies.agent_runner(...)` with no command subject and stores `command_subject: None`.
- `src/scherzo/domain.gleam` defines `UiRequestPolicy` as `Cancel`, `Fail`, `Ignore`, and `Operator`, and `PiConfig` includes `ui_request_timeout_ms`.
- `src/scherzo/agent/pi_rpc.gleam` exports `prompt`, `prompt_with_ui_policy`, `get_session_stats`, and `terminate`; lower-level turn-read helpers are still private. Its current `domain.Operator` UI policy branch returns `PiProtocolError("operator UI policy is not implemented")`.
- `src/scherzo/agent/pi_rpc.gleam` decodes `delta` for message updates but does not yet decode the JSON `message` field used by extension UI requests. This plan adds `RpcRecord.message` so operator UI prompts can show a redacted message excerpt.
- `test/fixtures/fake_pi_rpc.sh` supports transcript capture, delayed events, no output after prompt, UI dialog emission, and abort response, but it does not yet support waiting on a UI response inside a prompt turn or emitting a pi event before a command response.
- Before implementation began in this workspace, `direnv exec . gleam test` passed with 164 tests. After implementation, it passes with 175 tests.

If any of these facts differ, first normalize the tree to the current mutating-control implementation or update this plan before coding.

## Scope Boundaries

In scope: worker command subject creation; agent-level worker command and reply types; daemon worker-reply waiting and timeout handling; command-aware runner loop; stepwise pi RPC helpers that preserve skipped records; graceful abort; stop-after-current-turn; prompt queueing; prompt queue overflow and dropped-prompt events; operator UI pending state; UI response routing; UI timeout cancel; daemon handling of operator runner exits; fake pi fixture extensions; tests for runner, daemon, CLI-to-worker integration, and pi RPC helper parity; README updates.

Out of scope: Linear comment command transport; multi-user authorization; durable command audit storage; interactive stdin attach; editing files through the control API; global EventHub audit stream; persistent prompt queues across daemon restart; multiple simultaneous pending UI requests in one worker. If pi emits a second blocking UI request while one is pending, reject/fail the turn with `PiProtocolError("nested operator UI request")` and document it as unsupported.

## Milestones

Milestone 1 adds command plumbing without changing pi behavior. At the end, daemon-spawned workers have real command subjects, worker commands use an agent-level reply type, daemon routing waits for worker replies with the local control timeout, and tests can send fake worker replies without touching pi. Existing runner and daemon tests still pass.

Milestone 2 exposes stepwise pi RPC helpers. At the end, tests can send a prompt, read turn records with short timeouts, send abort, send UI cancel/value, and get stats while preserving `Session.next_id`. Helpers that wait for command responses return skipped pi records, and the existing high-level `pi_rpc.prompt` tests still pass.

Milestone 3 makes the runner command-aware for abort and stop-after-current-turn. At the end, abort sends pi `abort` when possible and exits with `OperatorAbort`; stop-after-current-turn replies `queued` during a turn, collects stats and refreshed issue state after `agent_end`, and exits with `OperatorStopAfterCurrentTurn`; the daemon parks issues for both without scheduling retries or reporting ordinary handoff failure.

Milestone 4 adds prompt queueing. At the end, prompts sent during an active turn return `queued`, are sent as the next prompt, respect a cap of 10, and produce dropped-prompt events if the worker exits before using them.

Milestone 5 adds operator UI. At the end, `pi.ui_request_policy: operator` waits for `RespondUi` or timeout, sends the correct pi UI response, enforces the UI timeout even when it is shorter than `pi.read_timeout_ms`, and updates session status/events. Existing `cancel`, `fail`, and `ignore` policies remain covered.

Milestone 6 updates docs and runs final acceptance. At the end, README explains which commands are graceful, which are queued, how UI timeout works, and how to validate with fake pi.

## Plan of Work

Create `src/scherzo/agent/worker_command.gleam`. Move the current daemon worker command shape there, but do not reuse `scherzo/control/command.CommandResult` as the worker reply type. Import `scherzo/control/command.UiResponse` for UI payloads, define `Command` variants `Abort`, `StopAfterCurrentTurn`, `QueuePrompt`, and `RespondToUi`, and define a separate `Reply` type with `Applied`, `Queued`, `Rejected`, `NotFound`, and `NotAllowed` variants. Each command variant carries `process.Subject(worker_command.Reply)`. Update `src/scherzo/orchestrator/daemon.gleam` to import this module and make `WorkerHandle.command_subject` an `Option(process.Subject(worker_command.Command))`.

Change `Message.ApplyOperatorCommand` and `apply_operator_command` in `src/scherzo/orchestrator/daemon.gleam` so the daemon actor receives the local control timeout as an `Int`. Change `handle_operator_command` to accept that timeout. Add a daemon helper that sends a `worker_command.Command`, waits on its reply subject for that timeout, and converts the `worker_command.Reply` into `control/command.CommandResult` using the original `OperatorCommand`. This conversion is the only place worker replies become public command results.

Change `RuntimeDependencies.agent_runner` in `src/scherzo/orchestrator/daemon.gleam` to accept one additional final argument: `process.Subject(worker_command.Command)`. Update `default_dependencies` to call the new command-aware runner function. Update every test dependency override to accept and ignore the extra command-subject argument unless the test is specifically exercising worker commands.

In `spawn_worker`, create `let command_subject = process.new_subject()` before spawning the worker process. Pass it to `dependencies.agent_runner`, and store `command_subject: Some(command_subject)` in `WorkerHandle`. Remove the local `daemon.WorkerCommand` type once all references use `worker_command.Command`.

In `src/scherzo/agent/pi_rpc.gleam`, add public stepwise helpers. Keep exact names stable for tests, and use these signatures:

    pub fn send_prompt(session: Session, message: String, read_timeout_ms: Int) -> Result(#(Session, List(RpcRecord)), error.PiRpcError)
    pub fn read_turn_record(session: Session, read_timeout_ms: Int, turn_deadline_ms: Int, stall_deadline_ms: Int) -> Result(#(Session, Option(RpcRecord)), error.PiRpcError)
    pub fn send_abort(session: Session, read_timeout_ms: Int) -> Result(#(Session, List(RpcRecord)), error.PiRpcError)
    pub fn send_extension_ui_cancel(session: Session, request_id: String, read_timeout_ms: Int) -> Result(#(Session, List(RpcRecord)), error.PiRpcError)
    pub fn send_extension_ui_value(session: Session, request_id: String, value: String, read_timeout_ms: Int) -> Result(#(Session, List(RpcRecord)), error.PiRpcError)

`send_prompt`, `send_abort`, and `get_session_stats` increment `next_id` after a successful response. `send_extension_ui_cancel` and `send_extension_ui_value` use the UI request id as the pi response id and do not consume `next_id`, matching the existing cancel behavior. The `List(RpcRecord)` in each command helper contains nonmatching pi records observed while waiting for the command response; callers must emit those records before continuing or exiting. `read_turn_record` treats `turn_deadline_ms` and `stall_deadline_ms` as absolute monotonic deadlines, not timeout durations. It returns `Ok(#(session, None))` only when the bounded read wait expires before any pi line and before either deadline; it returns `PiTurnTimeout` or `PiStallTimeout` when those absolute deadlines have passed. During pending operator UI, the runner intentionally passes the UI deadline as the temporary `stall_deadline_ms` and translates a resulting `PiStallTimeout` into the UI-timeout path instead of an ordinary stall failure. Add `encode_extension_ui_value_response` alongside `encode_extension_ui_response`; keep `encode_extension_ui_response` as the cancel encoder for compatibility.

Update `src/scherzo/agent/pi_rpc.gleam` decoding so `RpcRecord` includes `message: Option(String)`. Decode optional JSON field `message` into that field. In `src/scherzo/agent/runner.gleam`, make `update_from_record` use `record.delta` for `message_update` and `record.message` for `extension_ui_request` so operator UI events can show a redacted excerpt.

In `src/scherzo/error.gleam`, add `OperatorAbort` and `OperatorStopAfterCurrentTurn` to `AgentRunnerError`. Map them to `agent_operator_abort` and `agent_operator_stop_after_current_turn` in `agent_code`.

In `src/scherzo/agent/runner.gleam`, extend `WorkerFailure` so it carries `reason: error.AgentRunnerError`, `workspace_path: Option(String)`, `tokens: domain.TokenTotals`, and `final_issue: Option(domain.Issue)`. For existing failure paths before any pi stats are known, use `tokens: domain.zero_token_totals()` and `final_issue: None`. For stop-after-current-turn after a successful turn, collect session stats, emit `turn_finished`, refresh the issue, and return `Error(WorkerFailure(reason: error.OperatorStopAfterCurrentTurn, workspace_path: Some(workspace_path), tokens: totals, final_issue: Some(refreshed_issue)))`. For abort during an active turn, do not try to collect in-flight turn stats; return the totals from previously completed turns and `final_issue: None`.

In `src/scherzo/agent/runner.gleam`, add `run_attempt_with_commands` with the same arguments as `run_attempt` plus `command_subject`. Change `run_attempt` to create a fresh unused `worker_command.Command` subject and delegate to `run_attempt_with_commands`. Move the pi turn logic from the current recursive `loop_turns` into a command-aware loop that uses the new `pi_rpc` helpers. The loop computes absolute monotonic deadlines after each prompt acknowledgement: `turn_deadline_ms = now + config.pi.turn_timeout_ms` and `stall_deadline_ms = now + config.pi.stall_timeout_ms`. After each non-UI pi record, it resets the stall deadline to `now + config.pi.stall_timeout_ms`. The runner needs a private `monotonic_ms` external in `runner.gleam` or a small shared time helper; do not pass timeout durations into `read_turn_record` as if they were deadlines.

The runner loop state should include the current pi session, current issue, turn number, token totals, prompt queue, stop-after-turn flag, active-turn flag, optional pending UI request, effective config, tracker client, workspace path, and emit-update function. Keep this state private inside `runner.gleam`; do not add it to `domain.RuntimeState`.

Implement command handling in the runner. `Abort` sends pi abort when a session exists, emits `pi_abort_sent` or `pi_abort_failed`, emits any skipped pi records returned by `send_abort`, replies `worker_command.Applied(Some("abort sent"))`, terminates pi, runs `workspace.after_run`, emits dropped-prompt events for any queued prompts, and returns `Error(WorkerFailure(reason: error.OperatorAbort, workspace_path: Some(workspace_path), tokens: totals, final_issue: None))`. `StopAfterCurrentTurn` during a turn sets a flag, replies `worker_command.Queued(Some("stop requested after current turn"))`, and continues reading until `agent_end`; between turns it replies `worker_command.Applied(Some("stopped before next turn"))` and exits with `OperatorStopAfterCurrentTurn` before sending another prompt. `QueuePrompt` enforces the cap, redacts/truncates the message for events, and replies `Queued` while a turn is active. `RespondToUi` validates pending UI policy and request id, sends the pi response, emits skipped pi records from the helper, replies with `worker_command.Reply`, emits a UI response event, and clears pending UI state.

Implement pending operator UI timing in the runner, not in `pi_rpc.prompt_with_ui_policy`. When an `extension_ui_request` with method `select`, `confirm`, `input`, or `editor` arrives under `domain.Operator`, create pending UI state with deadline `now + config.pi.ui_request_timeout_ms`, emit the UI request event, and continue the loop. While that state exists, call the read helper with the UI deadline as the temporary activity deadline, and do not fail on the ordinary stall deadline. If the deadline expires, send `send_extension_ui_cancel`, emit skipped pi records, emit `operator_ui_timeout`, emit `extension_ui_response`, clear pending UI, reset `stall_deadline_ms`, and continue. If a second blocking UI request arrives while one is pending, fail the turn with `PiProtocolError("nested operator UI request")`.

Modify `finish_worker_failure` in `src/scherzo/orchestrator/daemon.gleam`. If `failure.reason` is `error.OperatorAbort`, finish the session with reason `operator_abort`, update the session hub with `failure.tokens`, park the issue through the same helper used by command fallback, cancel retry timers, release daemon state, and do not enqueue `ReportFailure`. If `failure.reason` is `error.OperatorStopAfterCurrentTurn`, do the same with reason `operator_stop_after_current_turn`, using `failure.final_issue` when present so the parked entry records the latest observed issue update time. Ordinary failures keep existing retry behavior and ordinary `ReportFailure` side effects.

The operator-exit side effects are intentionally different from ordinary failures. For `OperatorAbort`, the daemon publishes `worker_exited` with `operator_abort`, finishes the session with `operator_abort`, parks the original issue, cancels retry timers, removes running/session/monitor state, skips `ReportFailure`, and leaves workspace cleanup unchanged from the existing operator abort fallback. For `OperatorStopAfterCurrentTurn`, the daemon publishes `worker_exited` with `operator_stop_after_current_turn`, finishes the session with that reason, parks the refreshed issue when available, updates aggregate/session token totals from `failure.tokens`, cancels retry timers, removes running/session/monitor state, skips `ReportFailure`, and leaves workspace cleanup unchanged from the existing operator stop behavior. Existing ordinary worker failures continue to call `core.apply_worker_failure` and may schedule retries.

Modify daemon command handling. `PromptSession`, `RespondUi`, and `StopAfterCurrentTurn` should use the worker command subject when present, wait for the worker reply, and return the converted result. If no subject exists, `PromptSession` and `RespondUi` return `not_allowed(worker_command_subject_unavailable)`, and `StopAfterCurrentTurn` returns `not_allowed(worker_command_subject_unavailable)` because kill-and-park is abort semantics rather than stop-after-turn semantics. If these commands time out waiting for a worker reply, return `rejected(worker_command_timeout)`. `AbortSession` should send `worker_command.Abort` when a command subject exists and wait for the worker reply; if the subject is missing or the worker does not acknowledge within the command timeout, fall back to the existing kill-and-park safety path.

Extend `test/fixtures/fake_pi_rpc.sh`. Add a mode such as `FAKE_PI_UI_DIALOG_WAITS=1` where after emitting the blocking UI request, the script reads exactly one more JSON line from stdin, appends it to `FAKE_PI_TRANSCRIPT` if set, verifies it is `extension_ui_response`, emits its normal response JSON, then continues to `turn_end` and `agent_end`. Add a mode such as `FAKE_PI_INTERLEAVE_EVENT_BEFORE_COMMAND_RESPONSE=1` where the script emits a normal pi event before responding to `abort` or `extension_ui_response`, so helper tests can prove skipped records are returned. Add a mode such as `FAKE_PI_DELAY_BEFORE_AGENT_END_MS` if existing `FAKE_PI_DELAY_EVENT_MS` is not enough to create a deterministic active-turn window after `turn_start`.

Update `README.md` after implementation. Explain that abort is graceful when the worker command subject is present and falls back to kill-and-park only on timeout, prompts are queued between turns, UI operator mode waits up to `pi.ui_request_timeout_ms`, and `cancel`, `fail`, and `ignore` remain available policies.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and confirm the current baseline is green. Expected result before this plan's implementation is `154 passed, no failures`.

2. Create `src/scherzo/agent/worker_command.gleam` with `Command` and `Reply` types exactly as described in Interfaces and Dependencies. Do not edit daemon logic yet.

3. Add `test/agent_worker_command_test.gleam` with a compile-and-shape test that constructs each worker command variant and replies with `worker_command.Applied(Some("ok"))`, `Queued`, `Rejected("reason", None)`, `NotFound(None)`, and `NotAllowed("reason", None)` through a `process.Subject(worker_command.Reply)`.

4. In `src/scherzo/orchestrator/daemon.gleam`, import `scherzo/agent/worker_command`, remove the local `WorkerCommand` type, and update `WorkerHandle.command_subject` plus `stop_worker` to use `process.Subject(worker_command.Command)`.

5. Change `ApplyOperatorCommand` in `src/scherzo/orchestrator/daemon.gleam` to carry `timeout_ms: Int`. Change `apply_operator_command` so it sends that timeout in the actor message. Change `handle_message` and `handle_operator_command` to pass the timeout through.

6. Add a private daemon helper in `src/scherzo/orchestrator/daemon.gleam` that converts `worker_command.Reply` to `command.CommandResult` using the original `command.OperatorCommand`. Add unit coverage in `test/orchestrator_daemon_control_test.gleam` or an existing daemon test helper for `Applied`, `Queued`, `Rejected`, `NotFound`, and `NotAllowed`; assert the result target is the original session id for `PromptSession("session-1", "continue")`.

7. Update daemon `route_worker_command` to send a worker command, wait on the worker reply subject with `process.receive(reply, within: timeout_ms)`, convert the reply on success, and return `command.rejected(operator_command, "worker_command_timeout", Some("worker command timed out"))` on timeout. Do not change abort fallback yet.

8. Change `RuntimeDependencies.agent_runner` to accept a final `process.Subject(worker_command.Command)` argument. Update `default_dependencies` and every test override of `agent_runner` to accept the extra ignored argument. Run `direnv exec . gleam test`; expect the same tests to pass.

9. In `spawn_worker`, create a command subject, pass it into `dependencies.agent_runner`, and store `Some(command_subject)` in the `WorkerHandle`. Add a daemon control test in `test/orchestrator_daemon_control_test.gleam` that uses a fake `agent_runner` receiving the command subject, waits for a `QueuePrompt`, replies `worker_command.Applied(Some("prompt accepted"))`, and asserts `client.apply_command(control, command.PromptSession(session_id, "continue"))` returns status `applied` with target `Some(session_id)` instead of `not_allowed`.

10. Add a daemon timeout test where the fake worker receives `QueuePrompt` but does not reply. Call `apply_operator_command` with a small timeout and assert status `rejected` with reason `worker_command_timeout`.

11. Commit point: run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Commit the worker command plumbing if green.

12. Add tests to `test/pi_rpc_test.gleam` for `send_prompt` plus repeated `read_turn_record`. Use fake pi and assert `send_prompt` returns `#(session, [])`, the read records are `agent_start`, `turn_start`, `message_update`, `turn_end`, and `agent_end`, and `get_session_stats` after the turn returns totals of 3.

13. Add a `read_turn_record_uses_absolute_deadlines_test` to `test/pi_rpc_test.gleam`. Pass an already-expired turn deadline and assert `Error(error.PiTurnTimeout)`. Pass an already-expired stall deadline and assert `Error(error.PiStallTimeout)`. This proves callers must not pass timeout durations as deadlines.

14. Add a `decode_extension_ui_request_message_test` to `test/pi_rpc_test.gleam` that decodes `{"id":"ui-1","type":"extension_ui_request","method":"confirm","message":"continue?"}` and asserts `record.message == Some("continue?")`.

15. Add `send_abort_uses_next_command_id_test` to `test/pi_rpc_test.gleam`. Start fake pi with `FAKE_PI_TRANSCRIPT`, call `send_abort`, and assert the transcript contains an `abort` command after launch commands and that the returned skipped-record list is empty.

16. Extend `test/fixtures/fake_pi_rpc.sh` with `FAKE_PI_INTERLEAVE_EVENT_BEFORE_COMMAND_RESPONSE=1` for `abort` and `extension_ui_response`. When set, the fixture emits a JSON pi event before the command response.

17. Add `send_abort_returns_interleaved_records_test` to `test/pi_rpc_test.gleam`. Enable `FAKE_PI_INTERLEAVE_EVENT_BEFORE_COMMAND_RESPONSE=1`, call `send_abort`, and assert the returned skipped-record list contains the interleaved event rather than silently dropping it.

18. Add UI response helper tests to `test/pi_rpc_test.gleam`. Call `send_extension_ui_cancel(session, "ui-1", 1000)` and `send_extension_ui_value(session, "ui-2", "ok", 1000)` against fake pi and assert transcript lines contain `extension_ui_response`, `cancelled`, and the value payload for the value case.

19. Implement `RpcRecord.message`, `send_prompt`, `read_turn_record`, `send_abort`, `send_extension_ui_cancel`, `send_extension_ui_value`, skipped-record collection while waiting for command responses, and `encode_extension_ui_value_response` in `src/scherzo/agent/pi_rpc.gleam`. Keep `prompt` and `prompt_with_ui_policy` passing by rewriting them on top of the helpers or leaving their current implementation in place until parity is proven.

20. Commit point: run format check and tests. Commit stepwise pi RPC helpers if green.

21. Add `OperatorAbort` and `OperatorStopAfterCurrentTurn` to `error.AgentRunnerError` in `src/scherzo/error.gleam` and update `agent_code`. Add an error-code test in `test/agent_worker_control_test.gleam` or an existing error test file asserting the new codes are `agent_operator_abort` and `agent_operator_stop_after_current_turn`.

22. Extend `runner.WorkerFailure` in `src/scherzo/agent/runner.gleam` with `tokens` and `final_issue`. Update every existing `WorkerFailure(...)` constructor in `src/scherzo/agent/runner.gleam`, `src/scherzo/orchestrator/daemon.gleam`, and tests to pass `domain.zero_token_totals()` and `None` until command-aware paths fill real values. Run `direnv exec . gleam test`; expect only behavior-preserving updates.

23. Add `test/agent_worker_control_test.gleam`. Create helpers copied from `test/agent_runner_test.gleam` to build an `EffectiveConfig` with fake pi command, a workflow prompt, and a fake tracker. Keep test workspace directories under `test/tmp/agent-worker-control`.

24. In `src/scherzo/agent/runner.gleam`, add `run_attempt_with_commands` with the same arguments as `run_attempt` plus `command_subject`. Make `run_attempt` create an unused `process.Subject(worker_command.Command)` and delegate. At this step, `run_attempt_with_commands` may still call the existing implementation internally. Run tests.

25. Add a private `monotonic_ms` external or shared time helper in `src/scherzo/agent/runner.gleam`. Add small private helpers for computing absolute turn, stall, and UI deadlines. Do not change behavior yet.

26. Refactor the current pi turn loop in `src/scherzo/agent/runner.gleam` to use `pi_rpc.send_prompt` and `pi_rpc.read_turn_record` without polling worker commands yet. Emit any skipped records from `send_prompt`. Run existing `test/agent_runner_test.gleam` and `test/pi_rpc_test.gleam`; expect no behavior change.

27. Add `abort_command_stops_fake_pi_worker_test` to `test/agent_worker_control_test.gleam`. Start `runner.run_attempt_with_commands` in a spawned process with fake pi `FAKE_PI_NO_OUTPUT_AFTER_PROMPT=1` and transcript capture. Wait until transcript contains the first `prompt`, send `worker_command.Abort(reply)`, assert the reply is `worker_command.Applied`, assert the runner result is `Error(WorkerFailure(reason: error.OperatorAbort, tokens: domain.zero_token_totals(), final_issue: None, ...))`, and assert the transcript contains `abort`.

28. Add command polling to the runner loop for only `Abort`. Poll the command subject after each `read_turn_record` timeout, after each pi event, and before sending the next prompt. On abort, call `pi_rpc.send_abort`, emit any skipped records, reply `Applied`, terminate pi, run `workspace.after_run`, emit dropped-prompt events for any queued prompts, and return `OperatorAbort`. Run the abort test.

29. Add `stop_after_current_turn_exits_after_agent_end_test`. Use fake pi with a delayed event window, send `StopAfterCurrentTurn` while the turn is active, assert the command reply is `Queued`, assert the runner finishes with `OperatorStopAfterCurrentTurn`, assert `tokens.total == 3`, assert `final_issue` is `Some(...)`, and assert the transcript contains only one `prompt` command.

30. Implement stop-after-current-turn in the runner loop. During a turn, set a flag and continue to `agent_end`; after `agent_end`, collect stats, emit `turn_finished`, refresh issue state, run `workspace.after_run`, and return `OperatorStopAfterCurrentTurn`. Between turns, reply `Applied` and return the same reason before sending another prompt. Run tests.

31. Update `finish_worker_failure` in `src/scherzo/orchestrator/daemon.gleam` so `OperatorAbort` and `OperatorStopAfterCurrentTurn` park without ordinary retry or `ReportFailure`. Use `failure.final_issue` when present, update session tokens from `failure.tokens`, finish sessions with the operator reason, cancel retry timers, and remove running/session/monitor state.

32. Add daemon tests for operator exits in `test/orchestrator_daemon_control_test.gleam`. One test sends `AbortSession` to a fake command-aware worker and asserts the worker receives `Abort`, the command result is converted from the worker reply, the session exit reason is `operator_abort`, and the issue is parked without retry. Another test sends `StopAfterCurrentTurn` and asserts `operator_stop_after_current_turn`. Add an abort timeout test that proves no worker reply triggers kill-and-park fallback.

33. Commit point: run format check and tests. Commit graceful abort and stop-after-turn if green.

34. Add `operator_prompt_queued_during_turn_and_sent_next_turn_test` to `test/agent_worker_control_test.gleam`. Configure `max_turns: 2`, fake tracker to keep the issue active after the first turn, and fake pi transcript capture. Send `QueuePrompt("operator follow-up", reply)` while the first turn is active. Assert reply `Queued`, final transcript contains the operator follow-up as the second `prompt` message, and the automatic continuation prompt is not used for that second turn.

35. Add `operator_prompt_queue_full_test`. Start a long active turn, send ten `QueuePrompt` commands and assert `Queued`, send an eleventh and assert `Rejected("prompt_queue_full", _)`, then abort to end the worker cleanly.

36. Add `operator_prompt_dropped_on_exit_test`. Queue a prompt during a turn, then make the issue terminal after that turn so no next turn is sent. Assert emitted updates include `operator_prompt_dropped` with a redacted/truncated message.

37. Add the prompt queue field to the private runner loop state, initialized to `[]`.

38. Implement the `QueuePrompt` command handler in `src/scherzo/agent/runner.gleam`: validate the cap, push to the FIFO queue, emit `operator_prompt_queued`, and reply `Queued` or `Rejected("prompt_queue_full", _)`.

39. Implement next-prompt selection between turns. If an operator prompt is queued, pop it, emit `operator_prompt_sent`, send it via `pi_rpc.send_prompt`, and do not render the automatic continuation prompt for that turn.

40. Implement dropped-prompt auditing on all worker exits. For each unsent queued prompt, emit `operator_prompt_dropped` with redacted/truncated content. Run prompt queue tests.

41. Commit point: run format check and tests. Commit prompt queueing if green.

42. Extend `test/fixtures/fake_pi_rpc.sh` with `FAKE_PI_UI_DIALOG_WAITS=1`. In that mode, after emitting the UI request, read one `extension_ui_response` command from stdin, transcript it, return a success response for that same id, and then continue to `turn_end` and `agent_end`.

43. Add `operator_ui_request_cancel_response_test` to `test/agent_worker_control_test.gleam`. Configure `pi.ui_request_policy: domain.Operator`, fake pi `FAKE_PI_UI_DIALOG_WAITS=1`, and transcript capture. Wait until emitted updates include `extension_ui_request` with request id `ui-1` and message `continue?`; send `RespondToUi("ui-1", command.UiCancel, reply)`; assert reply `Applied`, transcript contains `extension_ui_response` and `cancelled`, emitted updates include `extension_ui_response`, and the runner completes the turn.

44. Add `operator_ui_request_value_response_test`. Use the same fake mode, send `RespondToUi("ui-1", command.UiValue("ok"), reply)`, and assert transcript contains the value payload.

45. Add `operator_ui_request_timeout_cancels_before_read_timeout_test`. Set `pi.ui_request_timeout_ms: 25` and `pi.read_timeout_ms: 1000`, do not send a response, and assert transcript contains a cancel response, emitted updates include `operator_ui_timeout`, and the runner finishes without waiting for the one-second read timeout.

46. Add `operator_ui_timeout_ignores_short_stall_timeout_test`. Set `pi.ui_request_timeout_ms: 100`, `pi.stall_timeout_ms: 10`, and fake pi UI wait mode. Do not send a response. Assert the result is not `PiStallTimeout`; it must emit `operator_ui_timeout` and cancel the UI request.

47. Add `operator_ui_wrong_request_id_rejected_test`. While `ui-1` is pending, send `RespondToUi("ui-wrong", command.UiCancel, reply)` and assert `Rejected("ui_request_not_pending", _)` or `NotFound`; then send the correct response and finish.

48. Add the pending UI state type in `src/scherzo/agent/runner.gleam` with request id, method, redacted message excerpt, created time, and absolute deadline. Do not yet send responses.

49. When `read_turn_record` returns a blocking `extension_ui_request` under `domain.Operator`, populate pending UI state, emit the UI request update using `record.message`, and continue the loop without applying ordinary stall timeout while pending.

50. Implement the `RespondToUi` command handler for pending UI: validate the request id, call `send_extension_ui_cancel` or `send_extension_ui_value`, emit skipped records, emit `extension_ui_response`, clear pending UI state, reset the stall deadline, and reply with `Applied` or `Rejected`.

51. Implement pending UI timeout handling. If the UI deadline is reached before a worker response, call `send_extension_ui_cancel`, emit skipped records, emit `operator_ui_timeout` and `extension_ui_response`, clear pending UI state, reset stall deadline, and continue reading the turn.

52. Preserve existing `cancel`, `fail`, and `ignore` behavior by keeping the current `test/pi_rpc_test.gleam` tests green and adding runner-level policy tests only if the refactor bypasses `pi_rpc.prompt_with_ui_policy` for daemon workers.

53. Update daemon integration tests so `scherzoctl ui respond` through `client.apply_command` reaches the worker subject and returns the daemon-converted command result. Use a fake runner for daemon routing and real fake pi for runner behavior.

54. Commit point: run format check and tests. Commit operator UI if green.

55. Update `README.md` and `docs/plans/mutating-operator-controls.md` to state that prompt queueing and operator UI are now implemented by command-aware workers. Mention queue cap 10, UI timeout, stall timeout behavior while UI is pending, daemon command timeout behavior, and abort fallback behavior.

56. Run final validation from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Expect all tests to pass. Record the final pass count in this plan's Progress and Outcomes sections.

57. Commit final documentation and acceptance fixes with a message such as `Add command-aware worker controls`.

## Testing and Falsifiability

This plan is falsified if a worker command can be accepted as successful without a worker command subject, if a worker reply can produce a `CommandResult` with the wrong command name or target session id, if a worker command timeout is reported as `queued` or `applied`, if a prompt command writes to pi while a turn is streaming, if an operator prompt is lost without an `operator_prompt_dropped` event, if prompt queue overflow does not return `rejected(prompt_queue_full)`, if `RespondUi` can answer the wrong request id, if `pi.ui_request_policy: operator` can hang beyond `pi.ui_request_timeout_ms`, if a UI timeout waits for the longer `pi.read_timeout_ms`, if the ordinary stall timeout fires while pi is intentionally waiting for operator UI input, if `cancel`, `fail`, or `ignore` regress, if graceful abort does not send an `abort` command when pi is responsive, if interleaved pi records are dropped while waiting for abort or UI-response acknowledgement, if operator abort or stop-after-current-turn schedules an ordinary retry or `ReportFailure`, or if any implementation writes to pi outside `src/scherzo/agent/pi_rpc.gleam`/the runner process that owns `pi_rpc.Session`.

Add or update tests in these files:

- `test/agent_worker_command_test.gleam` for command and reply type shape.
- `test/pi_rpc_test.gleam` for stepwise pi RPC helpers, message decoding, absolute deadline semantics, interleaved record preservation, and compatibility wrappers.
- `test/agent_worker_control_test.gleam` for runner command behavior against fake pi, including abort, stop-after-current-turn, prompt queueing, UI response, UI timeout shorter than read timeout, and UI timeout with short stall timeout.
- `test/orchestrator_daemon_control_test.gleam` for daemon routing, command subjects, worker reply conversion, command timeout handling, abort fallback, operator exit parking, and command results through the local control API.
- `test/config_test.gleam` only if UI config parsing changes again.
- `test/ctl_test.gleam` only if CLI command names or output changes.

Use `test/fixtures/fake_pi_rpc.sh` only; no test may call real pi or real Linear. Use transcript files under `test/tmp/...` to verify command order. Use fake daemon runners for daemon routing tests so the local control API path is covered without real pi. Use real fake-pi runner tests for the worker-owned protocol behavior.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Before implementation, the new worker-command tests should fail to compile or fail because `worker_command.gleam` does not exist and `command_subject` is `None`. After implementation, all tests should pass without sleeps longer than the fake pi delay windows required for deterministic active-turn tests. The UI timeout tests must use timeouts under 100 ms and still finish reliably.

## Validation and Acceptance

Acceptance is primarily automated because the repository test suite can exercise the local control API with fake daemon runners and fake pi without real Linear or real pi. From the repository root, run:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Accept only if the new daemon tests prove these control-path behaviors: `PromptSession` reaches a real worker command subject and returns a daemon-converted worker reply with target `Some(session_id)`; missing worker subjects return `worker_command_subject_unavailable`; worker command timeout returns `rejected(worker_command_timeout)` for prompt/UI/stop; abort timeout uses kill-and-park fallback; operator abort and stop-after-current-turn finish sessions with `operator_abort` and `operator_stop_after_current_turn` and do not schedule retries.

Accept only if the new runner/fake-pi tests prove these worker behaviors: a prompt sent during a turn is queued and becomes the next `prompt` command in the transcript; queue overflow returns `Rejected("prompt_queue_full", _)`; unsent prompts produce `operator_prompt_dropped`; graceful abort sends an `abort` command; stop-after-current-turn waits until `agent_end`, collects token stats, refreshes the issue, and exits before a second prompt; operator UI `--cancel` and `--value` send `extension_ui_response`; wrong UI ids are rejected; UI timeout sends cancel before a longer read timeout; and short stall timeout does not defeat operator UI waiting.

An optional manual smoke test can be run after the automated suite if a developer has a disposable Linear project and a workflow copied from `examples/WORKFLOW.md` with `pi.command` set to `test/fixtures/fake_pi_rpc.sh`, `pi.compatibility_probe: false`, `handoff.enabled: false`, and a short `polling.interval_ms`. Start the daemon from the repository root with:

    LINEAR_API_KEY=lin_api_... direnv exec . gleam run -- test/tmp/manual-command-aware-worker-loop/WORKFLOW.md

In another shell, run:

    scripts/scherzoctl sessions
    scripts/scherzoctl prompt <session-id> "summarize progress"
    scripts/scherzoctl abort <session-id> --yes
    scripts/scherzoctl session <session-id>

The expected visible results are command status `queued` or `applied` for prompt depending on turn timing, command status `applied` for abort, an `abort` command in the fake pi transcript, and session status `exited` with reason `operator_abort`. For operator UI smoke, start with `FAKE_PI_UI_DIALOG_WAITS=1` and `pi.ui_request_policy: operator`, attach to the session, then run:

    scripts/scherzoctl ui respond <session-id> ui-1 --cancel

Expect command status `applied`, a transcript line with `extension_ui_response`, and the session returning from `waiting_ui` to running or exited according to the fake turn. Do not accept this plan if prompt and UI commands only work through direct test hooks but still return `worker_command_subject_unavailable` through the daemon control path for real spawned workers.

## Rollout, Recovery, and Idempotence

This change is internal to local daemon workers and the local control API. It is additive from an operator perspective: existing read-only commands and scheduler commands continue to use the same control protocol and token. If command-aware worker behavior misbehaves, operators can still pause dispatch and use abort fallback if the daemon detects a worker command timeout.

Queued prompts and pending UI requests are runtime-only. They disappear when the worker exits or the daemon restarts. On worker exit, queued prompts are explicitly dropped and audited as session events. UI pending state resolves by response, timeout cancel, worker exit, or daemon shutdown.

Abort is idempotent only as an operator safety action. A second abort after the session exits should return `not_found` or a stable rejection because there is no running worker. Prompt and UI response commands are not globally idempotent; future Linear comment transport still needs its own deduplication before submitting commands to the daemon.

Rollback is straightforward: revert the worker-loop changes and the daemon will return to kill-and-park fallback or command-level `not_allowed` for worker commands. No durable data migration is involved.

## Artifacts and Notes

Expected command result examples after implementation. These are daemon-converted public command results, not worker-built results:

    {"ok":true,"data":{"command":"prompt","status":"queued","target":"ABC-123-42-1","message":"prompt queued for next turn"}}
    {"ok":true,"data":{"command":"respond_ui","status":"applied","target":"ABC-123-42-1","message":"ui response sent"}}
    {"ok":true,"data":{"command":"prompt","status":"rejected","target":"ABC-123-42-1","reason":"prompt_queue_full"}}
    {"ok":true,"data":{"command":"prompt","status":"rejected","target":"ABC-123-42-1","reason":"worker_command_timeout"}}

Expected fake pi transcript excerpt for queued prompt:

    {"id":"4","type":"prompt","message":"Original issue prompt ..."}
    {"id":"5","type":"get_session_stats"}
    {"id":"6","type":"prompt","message":"operator follow-up"}

Expected session event names for a queued prompt path:

    operator_prompt_queued
    operator_prompt_sent

Expected session event names for an operator UI timeout path:

    extension_ui_request
    operator_ui_timeout
    extension_ui_response

## Interfaces and Dependencies

In `src/scherzo/agent/worker_command.gleam`, define:

    import gleam/erlang/process
    import gleam/option.{type Option}
    import scherzo/control/command

    pub type Reply {
      Applied(message: Option(String))
      Queued(message: Option(String))
      Rejected(reason: String, message: Option(String))
      NotFound(message: Option(String))
      NotAllowed(reason: String, message: Option(String))
    }

    pub type Command {
      Abort(reply: process.Subject(Reply))
      StopAfterCurrentTurn(reply: process.Subject(Reply))
      QueuePrompt(message: String, reply: process.Subject(Reply))
      RespondToUi(
        request_id: String,
        response: command.UiResponse,
        reply: process.Subject(Reply),
      )
    }

In `src/scherzo/agent/runner.gleam`, keep `WorkerFailure` public but extend it to include accumulated metadata:

    pub type WorkerFailure {
      WorkerFailure(
        reason: error.AgentRunnerError,
        workspace_path: Option(String),
        tokens: domain.TokenTotals,
        final_issue: Option(domain.Issue),
      )
    }

In `src/scherzo/agent/runner.gleam`, expose:

    pub fn run_attempt_with_commands(
      issue: domain.Issue,
      attempt: Option(Int),
      workflow: domain.WorkflowDefinition,
      config: domain.EffectiveConfig,
      tracker_client: tracker.Client,
      emit_update: fn(String, PiUpdate) -> Nil,
      command_subject: process.Subject(worker_command.Command),
    ) -> Result(WorkerSuccess, WorkerFailure)

Keep the existing `run_attempt` signature and make it delegate to `run_attempt_with_commands` using a fresh unused subject.

In `src/scherzo/agent/pi_rpc.gleam`, extend `RpcRecord` with `message: Option(String)` and expose:

    pub fn send_prompt(session: Session, message: String, read_timeout_ms: Int) -> Result(#(Session, List(RpcRecord)), error.PiRpcError)
    pub fn read_turn_record(session: Session, read_timeout_ms: Int, turn_deadline_ms: Int, stall_deadline_ms: Int) -> Result(#(Session, Option(RpcRecord)), error.PiRpcError)
    pub fn send_abort(session: Session, read_timeout_ms: Int) -> Result(#(Session, List(RpcRecord)), error.PiRpcError)
    pub fn send_extension_ui_cancel(session: Session, request_id: String, read_timeout_ms: Int) -> Result(#(Session, List(RpcRecord)), error.PiRpcError)
    pub fn send_extension_ui_value(session: Session, request_id: String, value: String, read_timeout_ms: Int) -> Result(#(Session, List(RpcRecord)), error.PiRpcError)

The exact internal helper names may differ, but tests must exercise these public stepwise helpers without using private port functions.

In `src/scherzo/error.gleam`, extend `AgentRunnerError` with:

    OperatorAbort
    OperatorStopAfterCurrentTurn

In `src/scherzo/orchestrator/daemon.gleam`, update `Message.ApplyOperatorCommand` to carry the timeout and update the dependency shape to pass command subjects:

    ApplyOperatorCommand(
      command.OperatorCommand,
      timeout_ms: Int,
      process.Subject(command.CommandResult),
    )

    agent_runner: fn(
      domain.Issue,
      Option(Int),
      domain.WorkflowDefinition,
      domain.EffectiveConfig,
      tracker.Client,
      fn(String, runner.PiUpdate) -> Nil,
      process.Subject(worker_command.Command),
    ) -> Result(runner.WorkerSuccess, runner.WorkerFailure)

No new package dependencies are required.
