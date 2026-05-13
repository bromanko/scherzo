# Recover transient pi provider transport failures by letting pi retry the failed turn

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

Scherzo runs YAML workflow agent steps by launching pi, sending one rendered prompt,
and reading pi's JSON-RPC event stream until the agent turn is done. Today a transient
provider transport failure, such as a WebSocket disconnect reported by pi as an
assistant `stopReason="error"`, can become a terminal Scherzo workflow failure even
though pi already has built-in automatic retry logic for that exact class of problem.
After this change, Scherzo will keep the existing pi session alive long enough for pi
to retry the failed provider turn, and it will treat pi's `auto_retry_start` and
`auto_retry_end` events as part of the same logical YAML agent step session.

An operator should see fewer workflow failures caused only by temporary provider
transport instability. When pi retries and succeeds, the YAML step should complete
successfully in the same workspace, with one Scherzo session and one terminal
`YamlStepFinished`. When pi retries and exhausts its budget, Scherzo should fail the
workflow once, with session events and the final artifact making it clear that pi's
auto-retry was attempted and exhausted.

## Problem Framing and Constraints

The motivating incident was a YAML `workflow:implementation` run that failed during an
agent step with Scherzo error `agent_pi_failed`, underlying pi error
`pi_protocol_error`, provider diagnostic `provider_transport_failure`, and a WebSocket
error. The retained workspace contained useful partial work. Restarting the entire
workflow, rerunning a command step, or launching a fresh replacement agent as the first
recovery mechanism risks duplicating side effects and losing the most precise retry
boundary.

Pi already supports automatic retry for retryable assistant error messages. In the
pi implementation used by this repository, `AgentSession` recognizes retryable error
messages such as overloads, rate limits, HTTP 429/5xx responses, network or connection
errors, and WebSocket closes/errors. When pi decides to retry, it emits
`auto_retry_start`, removes the failed assistant error message from its in-memory
conversation state, waits with backoff, and calls `agent.continue()`. That is a better
boundary than a Scherzo daemon-level fresh-process retry because it preserves completed
tool results, avoids replaying tool calls, and retries the provider request rather than
the whole YAML step.

The current Scherzo problem is lower-level: `src/scherzo/agent/turn_loop.gleam` treats
any pi record with `stopReason="error"` as terminal immediately. That includes the
assistant error record that pi emits just before it emits `auto_retry_start`. The
terminal path calls `cleanup_failure` in `src/scherzo/agent/run_attempt.gleam`, which
terminates the pi process and runs workspace cleanup before pi can finish its own retry
sequence. This plan fixes that boundary.

This plan solves only transient pi/provider transport failures inside pi-backed YAML
workflow agent steps. It must not retry command steps, workflow hooks, workspace
preparation, template rendering, structured-output validation failures, operator aborts,
local pi launch failures, malformed pi JSON, pi process exits, context-window exhaustion,
or local read/turn/stall timeouts by variant alone. Those failures either already have
a separate recovery path, are deterministic, or need operator attention.

## Strategy Overview

Use pi's same-session auto-retry as the primary recovery mechanism. Scherzo should
observe retryable `stopReason="error"` records, defer terminal failure while pi decides
whether to retry, continue reading the same pi process through `auto_retry_start`, and
only finish the Scherzo turn when pi either succeeds or emits `auto_retry_end` with
`success: false`.

Add a small pure retry-event helper and make `turn_loop` retry-aware. The helper knows
how to classify retryable provider-transport protocol error details and how to parse
pi retry events from a `protocol.RpcRecord`. `turn_loop` keeps a pending retryable error
in its active-turn state instead of immediately calling `cleanup_failure`. When pi emits
`auto_retry_start`, Scherzo records the event and keeps reading. When pi later emits a
successful assistant completion, the normal success path finishes the turn. When pi
emits `auto_retry_end` with `success: false`, Scherzo converts the deferred error into
a single terminal `PiFailed(PiProtocolError(_))` failure and then uses the existing
cleanup path.

Keep the retry inside `src/scherzo/agent/turn_loop.gleam` and
`src/scherzo/agent/run_attempt.gleam`, not in `src/scherzo/orchestrator/daemon.gleam`.
The daemon's YAML wrapper owns Scherzo session registration and event publication, but
by the time `base.agent_step` returns `WorkerFailure`, `run_attempt.cleanup_failure`
has already terminated pi and run `workspace.after_run`. Retrying there is too late to
recover the failed provider turn safely.

Do not add `TransportRecoveryPrompt` as the default path. A fresh-process recovery
prompt may still be useful later as an explicit fallback when the original pi session
is gone and an operator accepts the side-effect risk, but it is not the first automatic
recovery mechanism for this incident class.

## Alternatives Considered

Doing nothing preserves today's safety but leaves operators with failed workflow runs
for transient provider instability even when pi knows how to retry.

Retrying the whole workflow from the beginning is too broad. It can rerun completed
workflow command steps, duplicate external side effects, prepare a different workspace,
and obscure the original provider failure class.

Retrying the whole YAML agent step from `run_yaml_agent_step` with a fresh
`TransportRecoveryPrompt` is also too broad as a primary mechanism. It starts after
`base.agent_step` returns, but at that point the lower-level runner has already
terminated pi and run cleanup. It also asks a new agent process to infer what happened
from workspace state rather than letting pi continue from its exact conversation state.

Sending a same-session natural-language follow-up such as "try again" is lighter than
a fresh process, but it is still weaker than pi's built-in retry. A normal follow-up
leaves the failed assistant error in context and relies on the model to self-correct.
Pi's `agent.continue()` retry removes the failed assistant message and reruns the
provider turn from the last valid user or tool-result message.

Adding a new Scherzo RPC command to tell pi to retry the last turn may be useful in a
future pi-facing change, but this plan does not need it. Pi already emits retry events
and runs `agent.continue()` internally; Scherzo only needs to stop terminating pi before
that sequence completes.

## Risks and Countermeasures

Scherzo could hang if it defers a retryable error but pi does not emit retry events.
The countermeasure is a short retry-decision grace deadline. Scherzo defers only when
`config.pi.auto_retry` is enabled and the protocol detail contains explicit retryable
evidence. After the failed `agent_end`, if neither `auto_retry_start` nor
`auto_retry_end` arrives before the grace deadline, Scherzo fails with the original
error instead of waiting for the normal long stall timeout.

Scherzo could mask semantic failures if the classifier is too broad. The countermeasure
is a pure classifier with positive tests for provider transport evidence and negative
tests for context exhaustion, launch failures, malformed JSON, process exits, local
timeouts, command failures, hook failures, and operator aborts. The classifier should
not retry all `agent_pi_failed` failures.

Retry observability could confuse operators if Scherzo records the transient error as a
terminal failed turn and then later records success. The countermeasure is to defer the
turn failure event until pi retry exhaustion is known. A retry that later succeeds
should produce one Scherzo `turn_finished` event, not an earlier `turn_failed` followed
by success.

Retry diagnostics could leak raw provider text. The countermeasure is to continue using
existing redaction for `RunnerPiUpdate.raw_json` and to keep new summary text limited to
machine-readable retry counts, outcomes, and reason codes. The raw provider error may
still appear in the existing final `PiProtocolError` detail path, which is already
redacted when artifacts and updates are built.

Pi's implementation may change. The countermeasure is to test against the JSON-RPC
contract documented and emitted by pi: `auto_retry_start` has `attempt`, `maxAttempts`,
`delayMs`, and `errorMessage`; `auto_retry_end` has `success`, `attempt`, and optional
`finalError`. If those events stop appearing, the new Scherzo tests fail and the grace
deadline falls back to the existing terminal failure behavior.

## Progress

- [x] (2026-05-13 00:00Z) Read the repo-local ExecPlan authoring guidance in `.pi/skills/exec-plan/SKILL.md`.
- [x] (2026-05-13 00:00Z) Inspected the current Scherzo pi client, turn loop, agent runner, YAML daemon wrapper, workflow runner, and pi event decoder paths.
- [x] (2026-05-13 00:00Z) Inspected pi's installed retry behavior and confirmed that pi emits `auto_retry_start` and `auto_retry_end` and retries with `agent.continue()`.
- [x] (2026-05-13 00:00Z) Revised this plan away from daemon-level fresh-process `TransportRecoveryPrompt` retry and toward same-session pi auto-retry.
- [x] (2026-05-13 21:40Z) Added pure pi auto-retry event parsing and retryable-provider-error classification in `src/scherzo/pi/retry_event.gleam`.
- [x] (2026-05-13 21:40Z) Taught Scherzo to recognize `auto_retry_start` and `auto_retry_end` pi events via `src/scherzo/agent/pi_event.gleam` and event publication.
- [x] (2026-05-13 21:45Z) Updated `turn_loop` to defer retryable assistant errors while pi auto-retry is deciding or running.
- [x] (2026-05-13 21:50Z) Added tests proving successful pi auto-retry stays in one Scherzo turn/session and does not call cleanup between attempts.
- [x] (2026-05-13 21:50Z) Added tests proving exhausted auto-retry fails once with useful diagnostics.
- [x] (2026-05-13 21:55Z) Added negative coverage proving semantic stop-reason errors and context-exhaustion recovery are not retried by this feature; the pure classifier covers local-process and timeout errors.
- [x] (2026-05-13 22:05Z) Validated formatting, tests, glinter, and Scherzo custom lint.

## Surprises & Discoveries

- Observation: Scherzo already enables pi auto-retry during launch.
  Evidence: `src/scherzo/pi/client.gleam` calls `send_auto_retry` from `complete_launch_handshake`, and `src/scherzo/config.gleam` defaults `pi.auto_retry` to `True`.

- Observation: Pi's auto-retry is a same-session, same-turn recovery. It removes the failed assistant error message and calls `agent.continue()` after emitting `auto_retry_start`.
  Evidence: The installed pi implementation's `AgentSession._handleRetryableError` emits `auto_retry_start`, removes the last assistant error message from `agent.state.messages`, sleeps, and calls `this.agent.continue()`.

- Observation: Scherzo currently terminates too early for this pi mechanism to work reliably. `turn_loop.stop_reason_failure` turns any `stopReason="error"` record into `PiProtocolError`, and the final failure path calls `cleanup_failure`, which terminates pi.
  Evidence: `src/scherzo/agent/turn_loop.gleam` calls `recoverable_or_final` from `handle_turn_record` as soon as `stop_reason_failure(record)` returns `Some(err)`. `src/scherzo/agent/run_attempt.gleam` terminates the pi session in `cleanup_failure`.

- Observation: Daemon-level retry is after the safe boundary. The YAML wrapper in `src/scherzo/orchestrator/daemon.gleam` only sees `WorkerFailure` after the lower-level runner has cleaned up the pi process.
  Evidence: `run_yaml_agent_step` delegates to `base.agent_step`; `base.agent_step` is backed by `run_attempt.run_prompt_mode_in_workspace`, whose failure path terminates the pi session before returning the `WorkerFailure`.

- Observation: Current `src/scherzo/agent/pi_event.gleam` does not have named constructors for pi retry events, so retry events currently fall through as `UnknownPiEvent`.
  Evidence: `PiEvent` contains message, tool, UI, compaction-recovery, and operator events, but not `AutoRetryStart` or `AutoRetryEnd`.

- Observation: The new retry state made `src/scherzo/agent/turn_loop.gleam` exceed the source guardrail if it stayed inline.
  Evidence: `gleam test` reported `src/scherzo/agent/turn_loop.gleam exceeds the new-module line threshold: 1287 > 1000` before extracting retry state and event-update helpers; after extraction, `turn_loop.gleam` is 998 lines and the source guardrail passes.

## Decision Log

- Decision: Primary recovery will use pi's existing same-session auto-retry, not a Scherzo daemon-level retry of the whole agent step.
  Rationale: Pi retries the failed provider turn with `agent.continue()` before tool calls are replayed. The daemon only sees failure after pi has already been terminated by lower layers.
  Date: 2026-05-13

- Decision: Implement retry-awareness in `src/scherzo/agent/turn_loop.gleam`.
  Rationale: This is the first Scherzo layer that sees the retryable assistant error and the later `auto_retry_start` / `auto_retry_end` events. It can defer terminal failure without closing pi.
  Date: 2026-05-13

- Decision: Keep `TransportRecoveryPrompt` out of the initial automatic recovery path.
  Rationale: A fresh process in the same workspace can still duplicate edits or external side effects. It is useful only as an explicit fallback after same-session retry is unavailable or exhausted.
  Date: 2026-05-13

- Decision: Add a retry-decision grace deadline.
  Rationale: If Scherzo defers a retryable-looking error but pi does not emit retry events, Scherzo must fail promptly instead of waiting for the long stall timeout.
  Date: 2026-05-13

- Decision: Extract retry state and turn-update formatting from `src/scherzo/agent/turn_loop.gleam` into `src/scherzo/agent/auto_retry.gleam` and `src/scherzo/agent/turn_update.gleam`.
  Rationale: Keeping the new behavior inline made `turn_loop.gleam` exceed the repository source guardrail. The extracted modules keep retry state pure, preserve existing event redaction behavior, and keep the turn loop below the 1000-line threshold without baselining a newly oversized module.
  Date: 2026-05-13

## Outcomes & Retrospective

Implemented. Pi provider transport failures are now deferred inside the active turn while pi decides whether to auto-retry. A successful pi auto-retry stays in one runner turn and one YAML step session, emits `auto_retry_start` and `auto_retry_end`, and does not send a second prompt or run cleanup between attempts. Exhausted pi auto-retry fails once with the existing public `agent_pi_failed` / `pi_protocol_error` classification and with retry lifecycle events preserved in the session stream. Non-retryable semantic stop-reason errors still fail immediately, and context-window exhaustion still follows the existing compaction recovery path without emitting auto-retry events.

Validation completed from the repository root with `direnv exec . gleam test` (`1277 passed, no failures`), `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint`. The lint commands report the repository's existing warning inventory but no errors.

## Context and Orientation

Scherzo is a workflow orchestrator. A YAML workflow is a directed graph of steps. A
command step runs a shell command in a prepared workspace. An agent step launches pi,
sends pi a rendered prompt, and lets pi inspect or edit that workspace. A workspace is
the worktree for the step. A Scherzo session is the operator-visible stream of events
for an agent or command step.

The main YAML workflow wrapper is in `src/scherzo/orchestrator/daemon.gleam`. The
function `yaml_workflow_dependencies` wraps the base `workflow_run.Dependencies`, and
`run_yaml_agent_step` registers one operator-visible session for a YAML agent step,
forwards `RunnerUpdate` values into the event hub, and finishes the session when the
base agent step returns.

The core workflow engine is in `src/scherzo/workflow_run.gleam`. It prepares
workspaces, calls `run_agent_invocation` once for an agent step, builds step artifacts,
and marks the workflow completed or failed. This plan keeps `run_agent_invocation` as
a single-invocation primitive; it should not grow a retry loop.

The pi runner is in `src/scherzo/agent/run_attempt.gleam`. It launches pi, sends the
prompt, delegates active event processing to `src/scherzo/agent/turn_loop.gleam`, and
terminates pi on failure. This plan changes the active event loop so retryable provider
errors are not treated as terminal before pi's own retry mechanism has a chance to run.

The pi JSON-RPC client is in `src/scherzo/pi/client.gleam`, and the JSON decoder is in
`src/scherzo/pi/protocol.gleam`. `client.launch_spec` already enables or disables pi
auto-retry by sending the `set_auto_retry` RPC command during launch. `protocol.RpcRecord`
contains the event `type_`, optional `stop_reason`, optional `error_message`, optional
`success`, tokens, tool fields, assistant messages, and `raw_json`.

The pi event name mapping is in `src/scherzo/agent/pi_event.gleam`. It maps pi JSON
`type` strings to Scherzo `PiEvent` constructors. Unknown pi events are still preserved
as `UnknownPiEvent(name)`, but this plan adds named retry events so tests and session
rendering can assert retry behavior directly.

Step artifacts are represented in `src/scherzo/step_artifact.gleam`. Agent failures
are converted to command-shaped failure artifacts by `agent_failure_artifact` in
`src/scherzo/workflow_run.gleam`. The initial implementation can expose retry details
through session events only, but exhausted retries should also add a short summary
suffix to the final failure artifact so retained artifacts explain why the step failed.

Relevant tests already exist in `test/agent_runner_test.gleam`, which uses
`test/fixtures/fake_pi_rpc.sh` to simulate pi JSON-RPC streams, and in
`test/orchestrator_daemon_session_event_test.gleam`, which checks session event
behavior for YAML workflow steps.

## Preconditions and Verified Facts

`src/scherzo/error.gleam` defines `PiRpcError` variants including `PiProtocolError`,
`PiReadTimeout`, `PiTurnTimeout`, `PiStallTimeout`, `PiExited`, `PiMalformedJson`,
`PiLaunchFailed`, and `PiContextWindowExhausted`. It maps `PiFailed(PiContextWindowExhausted)`
to `pi_context_window_exhausted` and other `PiFailed(_)` values to `agent_pi_failed`.

`src/scherzo/config.gleam` defaults `pi.auto_retry` to `True`, and
`src/scherzo/config/types.gleam` includes `auto_retry: Bool` in the pi configuration.

`src/scherzo/pi/client.gleam` sends `set_auto_retry` during `complete_launch_handshake`.
It sends a prompt with `send_prompt`, then `turn_loop` reads asynchronous pi records
with `read_turn_record` until the active turn ends or fails.

`src/scherzo/agent/turn_loop.gleam` currently checks every decoded record with
`stop_reason_failure(record)`. When it sees `stopReason="error"`, it returns a final
failure unless the error is context-window exhaustion. It does not currently understand
pi `auto_retry_start` or `auto_retry_end` events.

`src/scherzo/agent/run_attempt.gleam` terminates the pi session and runs
`workspace.after_run` in `cleanup_failure`. Any retry placed above this function but
after it returns is necessarily a fresh-process retry, not a same-session provider turn
retry.

The fake pi fixture for agent tests is `test/fixtures/fake_pi_rpc.sh`. It can already
emit prompt responses, `agent_start`, `turn_start`, `turn_end`, `agent_end`, context
exhaustion, stop-reason errors, tool messages, UI requests, and compaction events. This
plan extends that fixture to emit auto-retry events and a second successful assistant
completion without receiving a second `prompt` command.

Normal validation from the repository root is:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

If `direnv exec . <command>` reports that `.envrc` is blocked, read `.envrc`, run
`direnv allow .` from the repository root, and retry through `direnv exec .`.

## Scope Boundaries

In scope: recognizing pi auto-retry events, classifying retryable provider transport
protocol details, deferring terminal failure while pi auto-retry runs in the same
session, preserving one YAML step session across a successful retry, failing once after
pi retry exhaustion, and adding tests for positive, exhausted, and negative cases.

Out of scope: retrying whole workflows, retrying command steps, retrying workflow hooks,
changing workspace preparation, changing provider SDKs, adding a new operator parking
state, adding runtime retry configuration UI, or making fresh-process
`TransportRecoveryPrompt` the default recovery path.

A future follow-up may add an explicit operator-controlled fresh-process recovery prompt
for cases where the pi process is already gone and the operator accepts the risk of
resuming from workspace state. That is intentionally deferred until same-session retry
is working and measured.

## Milestones

Milestone 1 adds pure retry event parsing and classification. At the end of this
milestone, tests can prove that Scherzo recognizes pi `auto_retry_start` and
`auto_retry_end` records and classifies only explicit provider-transport protocol
errors as retry-deferrable.

Milestone 2 teaches the active turn loop to wait for pi auto-retry instead of
terminating immediately. At the end of this milestone, a fake pi stream can emit a
retryable error, `auto_retry_start`, a second successful assistant completion, and
`auto_retry_end(success: true)`, and Scherzo returns a normal `WorkerSuccess` without
calling cleanup between attempts.

Milestone 3 adds exhaustion and no-event safety. At the end of this milestone, a fake
pi stream can emit retryable failures until `auto_retry_end(success: false)`, and
Scherzo fails once with the original public error class. Another fake stream can emit a
retryable-looking error but no retry event, and Scherzo fails after the grace deadline
rather than waiting for the long stall timeout.

Milestone 4 verifies YAML session behavior. At the end of this milestone, daemon/session
tests prove that a retry which later succeeds remains one logical YAML step session,
keeps first-attempt events, and publishes one terminal `YamlStepFinished` only after the
final outcome.

Milestone 5 completes validation and cleanup. At the end of this milestone, tests,
formatting, glinter, and Scherzo custom lint pass, and this plan's Progress and
Outcomes sections reflect what was actually implemented.

## Plan of Work

Create a new module `src/scherzo/pi/retry_event.gleam`. Keep it pure. It should expose
a retryable-provider-error classifier and a parser for pi retry lifecycle events. The
classifier accepts `error.PiRpcError` or `error.AgentRunnerError` and returns `True`
only for `PiProtocolError(detail)` values containing explicit retryable evidence. Use
case-insensitive matching for strings that pi itself treats as retryable and that are
safe to defer: `provider_transport_failure`, `websocket error`, `websocket closed`,
`connection reset`, `connection refused`, `connection lost`, `econnreset`, `etimedout`,
`fetch failed`, `socket hang up`, `ended without`, `http2 request did not get a response`,
`terminated`, `overloaded`, `rate limit`, `too many requests`, `429`, `500`, `502`,
`503`, `504`, `service unavailable`, and `server error`. Do not classify
`PiReadTimeout`, `PiTurnTimeout`, `PiStallTimeout`, `PiLaunchFailed`, `PiMalformedJson`,
`PiExited`, or `PiContextWindowExhausted` as retryable by variant alone.

In `src/scherzo/pi/retry_event.gleam`, define a type similar to:

    pub type AutoRetryEvent {
      AutoRetryStart(
        attempt: Option(Int),
        max_attempts: Option(Int),
        delay_ms: Option(Int),
        error_message: Option(String),
      )
      AutoRetryEnd(
        success: Bool,
        attempt: Option(Int),
        final_error: Option(String),
      )
    }

Add `pub fn from_record(record: protocol.RpcRecord) -> Option(AutoRetryEvent)`. It may
parse `record.raw_json` with `scherzo/json_value.gleam` instead of adding more fields to
`protocol.RpcRecord`. For `auto_retry_end`, use `record.success` when present and parse
`finalError` from raw JSON for diagnostics.

Update `src/scherzo/agent/pi_event.gleam` with `AutoRetryStart` and `AutoRetryEnd`.
`to_string` must return `auto_retry_start` and `auto_retry_end`; `from_string` must map
those strings to the new constructors. Update `src/scherzo/orchestrator/event_publisher.gleam`
if needed so these events are treated as lifecycle or pi events rather than unknown raw
events. It is acceptable for the raw JSON to remain available on the event payload after
redaction.

Update `src/scherzo/agent/turn_loop.gleam`. Add an internal pending retry state to the
active-turn loop. The state should carry the deferred `PiRpcError`, whether pi has
actually emitted `auto_retry_start`, and an optional retry-decision deadline. Pass this
state through `active_turn_loop`, `handle_active_command`, `interpret_active_effects`,
`active_command_state`, `send_active_ui_response`, and `handle_turn_record` wherever
those functions currently pass the session, prompt queue, pending UI state, records,
and stall deadline.

Change `handle_turn_record` so `stop_reason_failure(record)` no longer immediately
returns `FinalFailure` for a retryable provider transport protocol error when
`context.config.pi.auto_retry` is true. Instead, store the error in pending retry state,
append the record to the current turn records, and continue reading. Non-retryable
`stopReason="error"` records keep the current behavior. Context-window exhaustion keeps
the existing context-recovery behavior and must not be routed through transport retry.

Change the `AgentEnd` branch in `handle_turn_record`. If no retry is pending, keep the
current behavior. If a retryable error is pending and pi has not yet emitted retry
exhaustion, do not return `ActiveTurn`; continue reading briefly for pi's retry decision.
Set a retry-decision deadline such as `monotonic_ms() + min_int(context.config.pi.read_timeout_ms, 1000)`.
If this deadline expires before `auto_retry_start` or `auto_retry_end`, fail with the
original deferred error. This prevents hangs when Scherzo's classifier is broader than
pi's actual retry decision.

Change the handling of `auto_retry_start`. When `retry_event.from_record(record)` returns
`AutoRetryStart`, clear any pending terminal failure, mark retry as active, emit the
normal `RunnerPiUpdate`, and keep reading the same pi process. Do not increment the
Scherzo workflow attempt index, do not call `cleanup_failure`, do not call
`workspace.after_run`, and do not send another prompt.

Change the handling of `auto_retry_end`. If it is `success: true`, clear retry state and
continue reading normally. If it is `success: false`, fail with the deferred error when
available, or with `PiProtocolError("pi auto-retry exhausted")` when no deferred error is
available. The final public Scherzo codes should remain `agent_pi_failed` and
`pi_protocol_error` for protocol transport failures.

Update `src/scherzo/workflow_run.gleam` only for final artifact summary text if needed.
If a failure reason is a pi protocol error caused by auto-retry exhaustion, append a
short bounded suffix such as ` pi_auto_retry=exhausted` to the artifact `summary_text`.
Do not add a broad retry loop to `run_agent_invocation`, and do not change workspace
preparation.

Update `test/fixtures/fake_pi_rpc.sh` with two new modes. `FAKE_PI_AUTO_RETRY_SUCCESS=1`
should, after the prompt response, emit a retryable assistant error sequence, emit
`auto_retry_start`, then emit a second successful assistant completion and
`auto_retry_end` with `success:true`, all without reading a second `prompt` command.
`FAKE_PI_AUTO_RETRY_EXHAUSTED=1` should emit a retryable assistant error sequence,
`auto_retry_start` events as appropriate, a final retryable assistant error sequence,
and `auto_retry_end` with `success:false` and `finalError`. Add a third mode such as
`FAKE_PI_RETRYABLE_ERROR_NO_RETRY_EVENT=1` that emits a retryable error and `agent_end`
but no retry event, to prove the grace deadline works.

Do not add production `let assert`, `panic`, or `todo`. Use explicit `Result` and
`Option` handling. If a lint suppression is unavoidable, follow the repository lint
policy and explain the narrow rule and reason on the line above the target.

## Concrete Steps

1. From the repository root, create `test/pi_retry_event_test.gleam`. Add tests for
   `pi_event.from_string("auto_retry_start")`, `pi_event.from_string("auto_retry_end")`,
   and `pi_event.to_string` for both new constructors. These tests should fail before
   `src/scherzo/agent/pi_event.gleam` is updated.

2. In `test/pi_retry_event_test.gleam`, add classifier tests that call the new pure
   classifier with `error.PiProtocolError("provider_transport_failure: WebSocket error")`,
   `error.PiProtocolError("ECONNRESET")`, `error.PiProtocolError("provider 503")`, and
   `error.PiProtocolError("429 rate limit")`. Assert that each is retryable.

3. In the same test file, add negative classifier tests for `PiReadTimeout`,
   `PiTurnTimeout`, `PiStallTimeout`, `PiMalformedJson("bad")`, `PiLaunchFailed("bad")`,
   `PiExited(2)`, and `PiContextWindowExhausted(provider: None, provider_code: None, detail: "context")`.
   Assert that none are retryable.

4. In `test/pi_retry_event_test.gleam`, add JSON parsing tests for records decoded from
   these strings:

       {"type":"auto_retry_start","attempt":1,"maxAttempts":3,"delayMs":2000,"errorMessage":"WebSocket error"}
       {"type":"auto_retry_end","success":true,"attempt":1}
       {"type":"auto_retry_end","success":false,"attempt":3,"finalError":"provider_transport_failure"}

   Assert that `retry_event.from_record` returns the expected `AutoRetryEvent` values.

5. Run the targeted new tests from the repository root:

       direnv exec . gleam test --target erlang test/pi_retry_event_test.gleam

   If the Gleam test runner in this repository does not support file-targeted runs, run
   `direnv exec . gleam test` instead. The new tests should fail before implementation
   because the module and pi events do not exist.

6. Create `src/scherzo/pi/retry_event.gleam` and implement the pure classifier and retry
   event parser described above.

7. Update `src/scherzo/agent/pi_event.gleam` with `AutoRetryStart` and `AutoRetryEnd`.
   Rerun `direnv exec . gleam test` and expect the new unit tests to pass or expose only
   the next missing wiring.

8. Update `test/fixtures/fake_pi_rpc.sh` with `FAKE_PI_AUTO_RETRY_SUCCESS=1`,
   `FAKE_PI_AUTO_RETRY_EXHAUSTED=1`, and `FAKE_PI_RETRYABLE_ERROR_NO_RETRY_EVENT=1`.
   Keep the existing fake behavior unchanged when those environment variables are not
   set.

9. In `test/agent_runner_test.gleam`, add `runner_allows_pi_auto_retry_to_succeed_in_same_turn_test`.
   Use `FAKE_PI_AUTO_RETRY_SUCCESS=1` with a transcript file. Assert that
   `runner.run_attempt` returns `Ok(success)`, `success.tokens.total` still comes from
   the final stats call, the transcript contains exactly one JSON command with
   `"type":"prompt"`, and the emitted Scherzo turn event names are exactly
   `["turn_started", "turn_finished"]`. Also assert that updates include
   `auto_retry_start` and `auto_retry_end`.

10. In the same test, assert that the workspace cleanup hook ran only once after final
    success. Use the existing `config(root, command, ..., ...)` helper if it creates
    `POPULATED` and `AFTER_RUN` markers, or add a local hook marker and count it. The
    key claim is that `workspace.after_run` did not run between the failed provider
    turn and pi's retry.

11. In `test/agent_runner_test.gleam`, add `runner_fails_once_when_pi_auto_retry_exhausts_test`.
    Use `FAKE_PI_AUTO_RETRY_EXHAUSTED=1`. Assert that `runner.run_attempt` returns
    `Error(failure)`, `error.agent_code(failure.reason) == "agent_pi_failed"`, the
    nested pi code is `pi_protocol_error`, updates include `auto_retry_start` and
    `auto_retry_end`, and Scherzo turn event names are exactly `["turn_started", "turn_failed"]`.

12. In `test/agent_runner_test.gleam`, add `runner_retryable_error_without_retry_event_fails_after_grace_test`.
    Use `FAKE_PI_RETRYABLE_ERROR_NO_RETRY_EVENT=1` and a small `read_timeout_ms` in the
    test config so the test stays fast. Assert that the runner fails with the original
    `PiProtocolError` instead of timing out with `PiStallTimeout`.

13. In `test/agent_runner_test.gleam`, keep or update the existing
    `runner_fails_when_pi_reports_stop_reason_error_test`. Change its fake error text,
    if needed, so it is clearly non-retryable. Assert that it still fails immediately
    with one `turn_failed` and no auto-retry events.

14. In `test/agent_runner_test.gleam`, add negative tests for `PiContextWindowExhausted`
    interaction if existing context-recovery tests do not already cover it. Assert that
    context exhaustion still emits `context_recovery_started` / `context_recovery_succeeded`
    and does not emit `auto_retry_start`.

15. Update `src/scherzo/agent/turn_loop.gleam` with pending retry state and event
    handling. Preserve existing operator UI, prompt queue, abort, stop-after-turn, and
    context-recovery behavior. When modifying function signatures, keep changes local to
    `turn_loop` where possible.

16. Rerun:

       direnv exec . gleam test

    Expect the new agent runner tests and existing runner tests to pass. If failures show
    a premature `turn_failed` before a successful retry, the turn loop is still treating
    retryable assistant errors as terminal too early.

17. In `test/orchestrator_daemon_session_event_test.gleam`, add a YAML-session test for
    successful pi auto-retry. Use the fake pi success mode through the normal daemon YAML
    workflow path. Assert that there is one logical YAML step session id, first-attempt
    pi events are preserved, `auto_retry_start` and `auto_retry_end` are present in that
    session, the session finishes normal, and exactly one `YamlStepFinished` is sent.

18. In the same daemon/session test file, add an exhausted-retry test. Assert that the
    session finishes failed once, retry events are present, the final worker failure still
    reports `agent_pi_failed` / `pi_protocol_error`, and there is no second YAML step
    session for the retry.

19. If artifact summary text is changed, add or update a test in `test/workflow_run_test.gleam`
    proving that exhausted pi auto-retry produces a failed step artifact whose
    `summary_text` contains `pi_auto_retry=exhausted` and whose `failure_code` remains
    compatible with existing pi protocol failures.

20. Run formatting from the repository root:

       direnv exec . gleam format --check src test

    If formatting fails, run the repository's normal formatter, inspect the diff, and
    rerun the check.

21. Run the full test suite:

       direnv exec . gleam test

    Expect all existing and new tests to pass.

22. Run production lint gates:

       direnv exec . gleam run -m glinter
       direnv exec . gleam run -m scherzo_lint

    Expect both commands to complete without new errors.

23. Update this plan's Progress, Surprises & Discoveries, and Outcomes & Retrospective
    sections with what actually happened.

24. Commit after the tree is green. A suitable commit message is:

       Let pi auto-retry transient provider transport failures

## Testing and Falsifiability

The same-session retry claim is falsified if a fake pi stream with
`FAKE_PI_AUTO_RETRY_SUCCESS=1` causes Scherzo to terminate pi, run workspace cleanup, send
a second prompt command, create a second YAML step session, emit `turn_failed`, or return
`WorkerFailure`. The new `agent_runner_test` and daemon/session tests must catch each of
those regressions.

The exhaustion claim is falsified if `FAKE_PI_AUTO_RETRY_EXHAUSTED=1` hangs until a
stall timeout, succeeds incorrectly, produces multiple terminal failures, or loses the
original public error class. Tests must assert one `turn_failed`, one failed session
finish, and final public codes `agent_pi_failed` and `pi_protocol_error`.

The no-semantic-retry claim is falsified if context exhaustion, local pi timeouts,
malformed JSON, launch failure, process exit, command failure, workflow hook failure, or
operator abort emits `auto_retry_start`, avoids its existing recovery path, or invokes
the agent more than the existing behavior expects. Classifier tests and existing runner
workflow tests must cover these cases.

The no-hang claim is falsified if a retryable-looking assistant error with no later
`auto_retry_start` or `auto_retry_end` waits until the long stall timeout. The fake
no-event test must use a small read timeout and assert that Scherzo fails with the
original deferred error after the grace period.

The observability claim is falsified if successful retries are invisible in the Scherzo
session event stream or if exhausted retries produce no hint that pi auto-retry was
attempted. Daemon/session tests must assert retry lifecycle events are present in the
session. If artifact summary text is added, workflow tests must assert the exhausted
summary suffix.

## Validation and Acceptance

From the repository root, validate this plan artifact with:

    scripts/scherzo-execplan validate docs/plans/LIV-261-recover-transient-pi-transport-failures.md

For implementation acceptance, run:

    direnv exec . gleam test
    direnv exec . gleam format --check src test
    direnv exec . gleam run -m glinter
    direnv exec . gleam run -m scherzo_lint

All commands should pass.

A YAML workflow agent step whose pi stream emits a retryable provider transport error,
then `auto_retry_start`, then a successful retry, and then `auto_retry_end(success:true)`
should complete successfully. The event stream should show one YAML step session, one
scheduled pi auto-retry inside that session, no terminal failure from the first provider
error, and one final `YamlStepFinished`.

A YAML workflow agent step whose pi stream emits retryable provider transport errors
until `auto_retry_end(success:false)` should fail once. The final failure should retain
`agent_pi_failed` and `pi_protocol_error`, and the session should show the pi auto-retry
lifecycle that led to exhaustion.

A non-retryable pi `stopReason="error"`, a context-window exhaustion error, a local
read timeout, a local turn timeout, a local stall timeout, a pi launch failure, malformed
pi JSON, a pi process exit, a command-step failure, a workflow hook failure, and an
operator abort should keep their existing behavior and should not be retried by this
feature.

## Rollout, Recovery, and Idempotence

The change is internal to pi-backed YAML workflow agent step execution. Existing
workflows do not need to opt in. The feature relies on the existing `pi.auto_retry`
configuration. Setting `pi.auto_retry` to `false` must preserve the old behavior: the
first retryable `stopReason="error"` becomes a terminal Scherzo failure.

Rollback is straightforward. Revert the `turn_loop` retry deferral and pi retry event
handling, or set `pi.auto_retry` to `false` in configuration. Because this plan does not
add a new workflow-level retry loop, rollback returns Scherzo to the previous first-failure
behavior without changing workspace preparation or scheduler state.

The same-session pi retry is more idempotent than fresh-process recovery because pi
continues from its transcript and completed tool results. It still cannot guarantee that
an external provider did not receive a request before the disconnect, but it avoids
rerunning workflow command steps and avoids asking a new agent to rediscover partial
workspace state.

If implementation stops after Milestone 1, only pure parsing and event mapping have
changed; behavior remains effectively unchanged. If implementation stops after Milestone
2 but before artifact summary improvements, successful same-session retry still works,
and exhausted failures still use existing terminal paths with retry events visible in
sessions.

## Artifacts and Notes

Useful source facts from the current tree:

    src/scherzo/pi/client.gleam
      complete_launch_handshake sends set_session_name, set_auto_retry, and get_state.
      send_prompt returns after the prompt command is accepted; asynchronous provider
      events are read later by turn_loop.

    src/scherzo/agent/turn_loop.gleam
      stop_reason_failure turns stopReason="error" into PiProtocolError.
      recoverable_or_final sends context exhaustion to context recovery and other pi
      errors to cleanup_failure.
      AgentEnd currently returns ActiveTurn when no operator UI request is pending.

    src/scherzo/agent/run_attempt.gleam
      cleanup_failure emits turn failure, terminates pi, runs workspace.after_run, drops
      queued prompts, and returns WorkerFailure.

    src/scherzo/orchestrator/daemon.gleam
      run_yaml_agent_step registers and finishes one YAML step session around
      base.agent_step. It is the right place to observe session-level results, but it is
      too late for same-session provider retry.

    test/fixtures/fake_pi_rpc.sh
      This fake pi process is the right fixture to simulate retryable JSON-RPC streams
      without relying on a real provider or real WebSocket failures.

## Interfaces and Dependencies

In `src/scherzo/pi/retry_event.gleam`, define:

    pub type AutoRetryEvent {
      AutoRetryStart(
        attempt: Option(Int),
        max_attempts: Option(Int),
        delay_ms: Option(Int),
        error_message: Option(String),
      )
      AutoRetryEnd(
        success: Bool,
        attempt: Option(Int),
        final_error: Option(String),
      )
    }

    pub fn from_record(record: protocol.RpcRecord) -> Option(AutoRetryEvent)

    pub fn retryable_pi_error(error: error.PiRpcError) -> Bool

    pub fn retryable_agent_error(error: error.AgentRunnerError) -> Bool

In `src/scherzo/agent/pi_event.gleam`, extend `PiEvent` with:

    AutoRetryStart
    AutoRetryEnd

In `src/scherzo/agent/turn_loop.gleam`, add an internal type similar to:

    type PendingAutoRetry {
      NoPendingAutoRetry
      PendingAutoRetry(
        error: error.PiRpcError,
        started: Bool,
        decision_deadline_ms: Option(Int),
      )
    }

The exact type name can differ, but it must stay private to `turn_loop` unless tests
need a pure helper. The behavior must be public through runner and daemon tests, not
through a broad new production API.

No new third-party dependency is required. Use existing Gleam standard library modules
and the existing `scherzo/json_value.gleam` helper for raw JSON parsing.

## Open Questions and Clarifications Needed

None for the first implementation. The fresh-process recovery prompt remains a possible
future operator-controlled fallback, but it should not block or complicate the
same-session pi auto-retry fix.
