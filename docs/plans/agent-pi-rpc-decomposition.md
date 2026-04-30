# Separate pi protocol, pi client IO, and agent turn-loop orchestration

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, the riskiest part of Scherzo's agent execution path should be easier to reason about and test. JSON protocol encoding/decoding for pi RPC should live in a protocol module with no process IO. Port launch, command sending, stdout reading, deadlines, and termination should live in a pi client module. Operator controls and active turn state should live in agent-specific modules. The existing high-level behavior should remain the same: Scherzo still prepares workspaces, renders prompts, optionally probes pi compatibility, starts pi, runs turns, handles operator prompts and blocking UI requests, accounts for tokens, refreshes issue state, classifies the final issue, and cleans up.

The visible proof is that `src/scherzo/agent/pi_rpc.gleam` no longer mixes JSON protocol logic with port/process management and turn event loops, and `src/scherzo/agent/runner.gleam` no longer contains the active turn loop, UI policy handling, operator command queueing, and high-level run composition in one 2,039-line file. From the repository root, `direnv exec . gleam test` must continue to pass. Existing fake-pi tests must still prove launch, prompt, abort, UI response, tool-event extraction, timeout, and stall behavior.

## Problem Framing and Constraints

The current `src/scherzo/agent/runner.gleam` is 2,039 lines. It defines public worker result/update types, prepares workspaces, renders templates, runs compatibility probes, launches pi, runs a multi-turn loop, handles worker commands from operators, implements blocking UI policy, accounts for tokens, refreshes tracker state, classifies final issues, and performs cleanup. The active turn loop starts around `active_turn_loop` and command/UI handling spans functions such as `handle_active_command`, `handle_turn_record`, `handle_extension_ui_record`, `handle_blocking_ui_policy`, `handle_ui_response_command`, `handle_operator_ui_timeout`, and `handle_abort_command`.

The current `src/scherzo/agent/pi_rpc.gleam` is 1,242 lines. It defines `Session`, a large loose `RpcRecord`, command JSON encoders, response decoders, process launch, prompt sending, read loops, timeout and stall handling, blocking UI request handling, token and tool extraction, port error mapping, and termination. `RpcRecord` is a wide record of optional fields because it tries to represent responses, lifecycle events, messages, tool calls, tool results, UI requests, session stats, and raw JSON in one type. `launch` is in the same module as JSON decoding. `read_events_until_agent_end` and `handle_blocking_ui_request` live beside protocol decoders.

This matters because pi protocol handling combines blocking IO, timeouts, external process failure, cancellation, operator intervention, and lossy JSON payloads. When protocol decoding, client transport, and agent turn policy are braided together, it is hard to test one concern without starting a fake process and hard to change one behavior without touching unrelated code.

The plan must preserve current runtime behavior and public daemon behavior. It must not change the pi wire protocol, fake-pi fixture expectations, EventHub update shape, worker command protocol, workspace preparation semantics, cleanup policy, or final issue classification. It may introduce compatibility facades during migration, but final production code should import the owner modules directly.

## Strategy Overview

Use a strangler refactor. First move pure protocol code out of `pi_rpc.gleam`; this is low risk because it has no IO. Then move port/client code into `src/scherzo/pi/client.gleam`, leaving a temporary `src/scherzo/agent/pi_rpc.gleam` facade so tests and callers can migrate gradually. Next move public agent result/update types into `src/scherzo/agent/types.gleam` and keep `runner.gleam` as a temporary facade. Then extract operator-control state and active turn-loop behavior from `runner.gleam`. Finally move high-level run composition into `src/scherzo/agent/run_attempt.gleam`, leaving `runner.gleam` either deleted or reduced to a small compatibility wrapper.

The desired final ownership is:

- `src/scherzo/pi/protocol.gleam`: encode/decode only. It should know JSON and pi record shapes, but it should not import `scherzo/port`, launch processes, read stdout, or implement operator policy.
- `src/scherzo/pi/client.gleam`: launch/send/read/terminate only. It should know the port abstraction, deadlines, timeout mapping, and protocol module, but it should not know Scherzo workspaces, tracker issues, template rendering, handoff, or final classification.
- `src/scherzo/agent/operator_control.gleam`: prompt queue, stop-after-turn flag, abort/queue/UI response command decisions, size guards, and pending UI state.
- `src/scherzo/agent/turn_loop.gleam`: the active turn finite-state machine. It reads records from `pi/client`, applies operator-control decisions, emits updates, and returns a turn result.
- `src/scherzo/agent/run_attempt.gleam`: high-level composition: workspace prep, prompt render, optional probe, pi launch, turn loop iteration, state refresh, classification, and cleanup.
- `src/scherzo/agent/types.gleam`: shared result/update types used by daemon, service, workflow run, handoff, result artifacts, and tests.

## Alternatives Considered

One alternative is to add comments or section headers to `runner.gleam` and `pi_rpc.gleam`. That does not reduce coupling or make protocol decoding testable without port IO.

Another alternative is a big-bang rewrite of the agent runner and pi client. That is too risky. The fake pi tests cover many edge cases, and a broad rewrite would make regressions difficult to isolate.

A third alternative is to keep all public names in `agent/pi_rpc.gleam` and only move private helpers. That reduces file size but preserves the misleading module boundary. This plan allows a temporary facade for migration, but final production code should import `scherzo/pi/protocol` and `scherzo/pi/client` directly.

A fourth alternative is to introduce an OTP actor for the pi client immediately. That may be useful later, but the current problem is module ownership. A process boundary would add ordering and supervision concerns before the simpler protocol/client/turn-loop split is proven.

## Risks and Countermeasures

The main behavior risk is changing timeout and stall semantics. Countermeasure: keep the existing fake-pi timeout tests in `test/pi_rpc_test.gleam` or their migrated equivalents, and add focused client tests before moving code. Do not change deadline calculations while extracting modules.

The main protocol risk is changing decoded `RpcRecord` fields such as tool name, tool input, tool output, tool status, assistant messages, tokens, or raw JSON. Countermeasure: move protocol tests first and keep assertions unchanged for captured fixtures such as `test/fixtures/pi_tool_events_captured.jsonl`.

The main operator-control risk is changing reply timing or queue semantics. Countermeasure: preserve tests in `test/agent_worker_control_test.gleam`, especially abort, queued prompt, UI response, UI timeout, and prompt-too-large behavior. Add direct unit tests for the extracted `operator_control` module.

The main compatibility risk is changing imports used by daemon, service, workflow run, handoff, and tests. Countermeasure: introduce `agent/types.gleam` and temporary facade modules. Migrate production imports one group at a time. Keep `runner.run_attempt`, `runner.run_attempt_with_commands`, and `runner.run_attempt_with_command_ready` available until all production callers use `agent/run_attempt.gleam`.

The main import-cycle risk is letting `pi/client.gleam` import agent modules or letting `pi/protocol.gleam` import client modules. Countermeasure: dependencies must point one way: protocol is lowest; client imports protocol; agent turn loop imports client and operator control; run attempt imports turn loop; runner facade imports run attempt.

## Progress

- [x] (2026-04-30 10:47Z) Verified the current test baseline earlier in this work session with `direnv exec . gleam test`; it passed with `377 passed, no failures`.
- [x] (2026-04-30 12:15Z) Fact-checked current file sizes: `src/scherzo/agent/runner.gleam` has 2,039 lines and `src/scherzo/agent/pi_rpc.gleam` has 1,242 lines.
- [x] (2026-04-30 12:20Z) Reviewed current public functions and tests in `pi_rpc.gleam`, `runner.gleam`, `test/pi_rpc_test.gleam`, `test/agent_runner_test.gleam`, and `test/agent_worker_control_test.gleam`.
- [ ] Milestone 0: add characterization tests and record baseline behavior for protocol, client IO, and operator controls.
- [ ] Milestone 1: extract pi protocol encoding/decoding to `src/scherzo/pi/protocol.gleam`.
- [ ] Milestone 2: extract pi client IO/deadline behavior to `src/scherzo/pi/client.gleam`.
- [ ] Milestone 3: move shared agent result/update types to `src/scherzo/agent/types.gleam`.
- [ ] Milestone 4: extract operator-control state and decisions to `src/scherzo/agent/operator_control.gleam`.
- [ ] Milestone 5: extract the active turn FSM to `src/scherzo/agent/turn_loop.gleam`.
- [ ] Milestone 6: extract high-level run composition to `src/scherzo/agent/run_attempt.gleam` and reduce `runner.gleam` to a facade or remove it.
- [ ] Milestone 7: migrate imports, remove facades if possible, run structural checks, and write the retrospective.

## Surprises & Discoveries

- Observation: Current production code does not construct `pi_rpc.RpcRecord(...)` or `pi_rpc.Session(...)` directly.
  Evidence: searches for `RpcRecord(` and `pi_rpc.Session(` in `src` and `test` returned no matches. This makes temporary type aliases in a facade feasible because callers use field access and function returns, not constructors.

- Observation: `runner.gleam` already uses a stepwise pi RPC API for the active command-aware loop.
  Evidence: production runner calls `pi_rpc.launch`, `pi_rpc.send_prompt`, and `pi_rpc.read_turn_record`; the higher-level `pi_rpc.prompt` and `prompt_with_ui_policy` helpers are used by tests, not by the runner.

- Observation: The existing tests are well positioned for this refactor.
  Evidence: `test/pi_rpc_test.gleam` already separates codec assertions, stepwise prompt reads, deadlines, abort/UI helper commands, launch/prompt/stats, tool event decoding, malformed JSON, and timeout/stall scenarios. `test/agent_worker_control_test.gleam` already exercises command-aware runner behavior.

## Decision Log

- Decision: Keep a temporary `src/scherzo/agent/pi_rpc.gleam` facade while migrating callers.
  Rationale: Existing tests and modules import `scherzo/agent/pi_rpc`. A facade lets protocol and client extraction land in small green commits. Final production imports should move to `scherzo/pi/protocol` and `scherzo/pi/client` before completion.
  Date: 2026-04-30

- Decision: Move shared worker result/update types to `src/scherzo/agent/types.gleam` before extracting turn loop and run attempt.
  Rationale: Daemon, service, workflow run, handoff, result artifact, and tests use `runner.WorkerSuccess`, `runner.WorkerFailure`, and `runner.PiUpdate`. A neutral types module avoids import cycles when `turn_loop` and `run_attempt` are split.
  Date: 2026-04-30

- Decision: Do not introduce a pi client actor in this plan.
  Rationale: The immediate problem is module entanglement. A new process boundary would add supervision, mailbox ordering, and shutdown semantics before the simpler split is proven.
  Date: 2026-04-30

- Decision: Preserve current `RpcRecord` shape for the first protocol extraction.
  Rationale: The wide optional record is a real smell, but changing it while moving modules would combine protocol behavior changes with extraction. First move it unchanged, then optionally split response/event/tool records in a later plan or final cleanup milestone if tests make it low risk.
  Date: 2026-04-30

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

Scherzo runs pi through an RPC-style subprocess. The current low-level module is `src/scherzo/agent/pi_rpc.gleam`. It starts a port with `scherzo/port`, writes JSON command lines to stdin, reads JSONL records from stdout, decodes them into `RpcRecord`, maps port errors to `error.PiRpcError`, and terminates the process. It also currently includes protocol encoders and decoders.

The current high-level agent runner is `src/scherzo/agent/runner.gleam`. Its public functions are `run_attempt`, `run_attempt_with_commands`, `run_attempt_with_command_ready`, and `run_prompt_in_workspace`. It also exports `FinalClassification`, `WorkerSuccess`, `WorkerFailure`, and `PiUpdate`. The daemon starts workers through `runner.run_attempt_with_command_ready`, workflow YAML agent steps use runner dependencies, and tests import runner result types.

Worker commands are defined in `src/scherzo/agent/worker_command.gleam`. The runner receives commands through a `process.Subject(worker_command.Command)` while a turn is active or between turns. Operator commands can abort, request stop after current turn, queue another prompt, or respond to a pending UI request.

Pi RPC fake-process coverage comes from `test/fixtures/fake_pi_rpc.sh` and tests under `test/pi_rpc_test.gleam`, `test/agent_runner_test.gleam`, and `test/agent_worker_control_test.gleam`. These tests are the safety net for this refactor.

## Preconditions and Verified Facts

Before implementation starts, run from the repository root:

    jj status
    direnv exec . gleam test
    wc -l src/scherzo/agent/runner.gleam src/scherzo/agent/pi_rpc.gleam

At plan authoring, the file-size output was:

    2039 src/scherzo/agent/runner.gleam
    1242 src/scherzo/agent/pi_rpc.gleam
    3281 total

The baseline test output earlier in this work session was:

    377 passed, no failures

Use these searches to normalize if the tree has changed:

    grep -R "^import scherzo/agent/pi_rpc" -n src test --include='*.gleam'
    grep -R "^import scherzo/agent/runner" -n src test --include='*.gleam'
    grep -n "^pub type \|^type \|^pub fn \|^fn " src/scherzo/agent/pi_rpc.gleam
    grep -n "^pub type \|^type \|^pub fn \|^fn " src/scherzo/agent/runner.gleam

At plan authoring, production imports of `agent/pi_rpc` were `src/scherzo/agent/probe.gleam`, `src/scherzo/agent/runner.gleam`, and `src/scherzo/result_artifact.gleam`; tests also import it. Production imports of `agent/runner` included handoff, handoff formatting, orchestrator daemon/service, step artifact, and workflow run.

This repository uses Jujutsu. Do not use mutating `git` commands. Use `jj status`, `jj describe -m "message"`, and `jj new` for milestone commit discipline.

## Scope Boundaries

In scope: extracting pure pi protocol encoding/decoding; extracting pi client process IO and deadlines; migrating pi RPC tests to protocol/client modules; moving shared agent types; extracting operator prompt/UI/abort control decisions; extracting the active turn FSM; extracting high-level run-attempt composition; shrinking or removing `runner.gleam` and `pi_rpc.gleam` facades.

Out of scope: changing the pi JSON protocol; changing fake pi fixture behavior; changing worker command message variants; changing EventHub payload shape; changing token accounting semantics; changing workspace hook behavior; changing final issue classification rules; introducing supervision/OTP actors for pi; splitting the large `RpcRecord` into a richer event ADT beyond what is required for extraction.

If `docs/plans/typed-internal-state.md` has already introduced `PiEvent` or typed step/status values, use those types in the new modules instead of reintroducing raw strings. If it has not landed, preserve the current string fields and let that later plan type them.

## Milestones

Milestone 0 adds characterization tests and records current behavior. At the end, tests prove the current protocol strings, decoded record fields, fake-pi IO, timeout/stall behavior, and operator-control behavior before any extraction.

Milestone 1 creates `src/scherzo/pi/protocol.gleam`. At the end, JSON command encoding and record decoding live there, and `agent/pi_rpc.gleam` delegates encoding/decoding to the new module.

Milestone 2 creates `src/scherzo/pi/client.gleam`. At the end, process launch, command sending, response collection, turn-record reading, timeouts, stall deadlines, session stats, and termination live there. `agent/pi_rpc.gleam` is a small compatibility facade.

Milestone 3 creates `src/scherzo/agent/types.gleam`. At the end, `FinalClassification`, `WorkerSuccess`, `WorkerFailure`, and `PiUpdate` live there, and `runner.gleam` no longer needs to be imported solely for types.

Milestone 4 creates `src/scherzo/agent/operator_control.gleam`. At the end, prompt queue manipulation, stop-after-turn state, pending UI state, size guards, and command reply decisions are outside `runner.gleam`.

Milestone 5 creates `src/scherzo/agent/turn_loop.gleam`. At the end, the active turn loop and blocking UI policy handling are outside `runner.gleam`, and they depend only on pi client, operator control, worker commands, config, and update emission.

Milestone 6 creates `src/scherzo/agent/run_attempt.gleam`. At the end, workspace preparation, template rendering, compatibility probing, pi launch, multi-turn orchestration, state refresh, final classification, and cleanup are composed there. `runner.gleam` becomes a wrapper or is deleted after all imports move.

Milestone 7 migrates imports, removes obsolete facades if possible, runs final structural checks, and records outcomes.

## Plan of Work

Begin by moving pure protocol code because it is easiest to validate and has the lowest risk. Then move client IO while preserving the public `pi_rpc` API through a facade. Next move shared types so later agent modules can import them without depending on `runner.gleam`. Extract operator control before turn loop so the turn loop can call a smaller API instead of carrying every queue/UI/reply detail itself. Extract the active turn loop before high-level run-attempt composition. Finally, reduce `runner.gleam` to a facade that calls `agent/run_attempt.gleam`, then update production imports to the new modules.

Each milestone should be behavior-preserving. When a function moves, copy it first, call it from the old place, run tests, then delete the old implementation. Avoid simultaneous semantic cleanup. The only acceptable behavior changes in this plan are test-only import changes and module ownership changes.

## Concrete Steps

1. From the repository root, run `jj status`. If unrelated source changes exist, stop and decide whether to move to a clean workspace or record them in this plan.

2. Run `direnv exec . gleam test`. Expect `no failures`. At plan authoring, the suite reported `377 passed, no failures`.

3. Add characterization tests before moving code. In `test/pi_rpc_test.gleam`, ensure existing tests cover command encoding, response decoding, extension UI request decoding, assistant message extraction, captured tool call/result extraction, launch/prompt/stats with fake pi, stepwise prompt/read, abort/UI helpers, malformed JSON, read timeout, stall timeout, and turn timeout. If any of those assertions are missing after tree drift, add them now.

4. Add one test in `test/agent_worker_control_test.gleam` if not already present that queues a prompt while a turn is active and verifies the transcript contains a second prompt after the current turn. Add one test for operator UI timeout if not already present. Run `direnv exec . gleam test` and record the pass count in Progress.

5. Create directory `src/scherzo/pi/` if it does not exist. Create `src/scherzo/pi/protocol.gleam`.

6. Move these protocol-only definitions from `src/scherzo/agent/pi_rpc.gleam` to `src/scherzo/pi/protocol.gleam`: `RpcRecord`, command encoders `encode_set_session_name`, `encode_set_auto_retry`, `encode_get_state`, `encode_prompt`, `encode_abort`, `encode_get_session_stats`, `encode_extension_ui_response`, `encode_extension_ui_value_response`, `decode_record`, private decoder types `MessageObject`, `AgentEndMessage`, `ContentItem`, `Data`, decoder helpers, token decoder, tool extraction helpers, and any JSON-only helper required by decoding.

7. In `src/scherzo/agent/pi_rpc.gleam`, import `scherzo/pi/protocol`. Replace moved implementations with forwarding functions. Use type aliases for `RpcRecord` and `Data` if needed. Because current source and tests do not construct `pi_rpc.RpcRecord(...)`, a type alias is safe during migration.

8. Create `test/pi_protocol_test.gleam` and move protocol-only tests from `test/pi_rpc_test.gleam`: command encoder checks, decode response/event, extension UI request message, assistant messages, captured tool call/tool result, and top-level/data tool execution aliases. Update imports to `scherzo/pi/protocol`.

9. Run:

    grep -n "decode_record\|record_decoder\|encode_prompt\|MessageObject" src/scherzo/agent/pi_rpc.gleam
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    The grep may show forwarding functions in `pi_rpc.gleam`, but JSON decoder implementation details such as `record_decoder` and `MessageObject` should live only in `pi/protocol.gleam`. Record the milestone with `jj describe -m "Extract pi protocol codec"` and start a new change with `jj new` if keeping milestones separate.

10. Create `src/scherzo/pi/client.gleam`. Move `Session`, `launch`, `send_prompt`, `read_turn_record`, `send_abort`, `send_extension_ui_cancel`, `send_extension_ui_value`, `get_session_stats`, `terminate`, `send_expect_success`, `send_auto_retry`, `send_get_state`, response collection, skipped-record limits, deadline calculations, port error mapping, `try_pi`, `int_to_string`, and `monotonic_ms` from `agent/pi_rpc.gleam` into `pi/client.gleam`. The client module should import `scherzo/pi/protocol`, `scherzo/port`, `scherzo/error`, and low-level stdlib modules only.

11. Move `prompt` and `prompt_with_ui_policy` into `pi/client.gleam` only as compatibility helpers if tests still need them. Mark them in comments as high-level legacy helpers because blocking UI policy should move to `agent/turn_loop.gleam`. Do not let new production code call them.

12. In `agent/pi_rpc.gleam`, replace client implementations with forwarding functions to `pi/client.gleam`. Use `pub type Session = client.Session` as a temporary alias. Keep public function signatures unchanged for this milestone.

13. Create `test/pi_client_test.gleam` and move client/port/fake-pi tests from `test/pi_rpc_test.gleam`: stepwise prompt read and stats, absolute deadlines, abort/UI response helpers, launch/prompt/stats, fake tool events, malformed JSON and timeout, short read timeouts, stall timeout, turn timeout, UI policy legacy helper tests if kept, and diagnostics tests. Update imports to `scherzo/pi/client` and `scherzo/pi/protocol` as appropriate.

14. Run:

    grep -n "port\.\|read_turn_line\|read_until_response\|launch(" src/scherzo/agent/pi_rpc.gleam
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    `agent/pi_rpc.gleam` should have no direct `port.` calls or read-loop implementations. Record the milestone.

15. Create `src/scherzo/agent/types.gleam`. Move `FinalClassification`, `WorkerSuccess`, `WorkerFailure`, and `PiUpdate` from `runner.gleam` into it. Update `runner.gleam` to import `agent/types` and either alias the types temporarily or use them directly. Since external code does not construct these through aliases in source except record constructors may exist in tests, search before choosing aliases.

16. Run searches:

    grep -R "runner\.WorkerSuccess\|runner\.WorkerFailure\|runner\.PiUpdate\|runner\.Final" -n src test --include='*.gleam'
    grep -R "WorkerSuccess(" -n src test --include='*.gleam'

    Update imports in production modules that only need types to `import scherzo/agent/types as agent_types`. If many tests construct `runner.WorkerSuccess(...)`, leave `runner.gleam` constructors in place until a focused test migration commit, or update those tests in the same green slice.

17. Run format and tests. Record the milestone with `jj describe -m "Move shared agent result types"`.

18. Create `src/scherzo/agent/operator_control.gleam`. Move or recreate these concepts from `runner.gleam`: `PendingUi`, prompt queue max length, operator prompt max-size guard, UI response max-size guard, queue prompt decision, stop-after-turn decision, pending UI response decision, abort decision result shape, and helper functions for replying with `worker_command.Reply`.

19. Design the operator-control API to be mostly pure. A concrete shape is:

    pub type State {
      State(
        prompt_queue: List(String),
        stop_after_turn: Bool,
        pending_ui: Option(PendingUi),
      )
    }

    pub type Decision {
      Continue(State)
      AbortRequested
      SendUiCancel(request_id: String)
      SendUiValue(request_id: String, value: String)
    }

    pub fn handle_command(state: State, command: worker_command.Command) -> #(State, Decision)

    If direct reply sending is simpler, keep reply sending in this module but do not let it know about pi sessions, tracker clients, workspace cleanup, or final classification.

20. Add `test/agent_operator_control_test.gleam`. Test prompt too large, prompt queue full, prompt queued, stop-after-current-turn, UI response with no pending request, UI response wrong request id, UI cancel decision, UI value decision, and abort decision. These tests should not start fake pi.

21. Update `runner.gleam` active command handling to call `operator_control` for queue/UI/reply decisions while preserving existing emitted updates and cleanup behavior. Keep side effects such as `pi_client.send_extension_ui_cancel` in runner for this milestone if moving them immediately would be too large.

22. Run format and tests. Record the milestone.

23. Create `src/scherzo/agent/turn_loop.gleam`. Move `ActiveCommandState`, `ActiveTurn`, `active_turn_loop`, `handle_active_command`, `handle_turn_record`, `handle_extension_ui_record`, `handle_blocking_ui_policy`, `handle_ui_response_command`, `handle_operator_ui_timeout`, `handle_abort_command`, `emit_records`, `is_blocking_ui_method`, `try_active`, and update-construction helpers only if they are turn-loop-specific. The module should import `scherzo/pi/client`, `scherzo/pi/protocol`, `scherzo/agent/operator_control`, `scherzo/agent/types`, `scherzo/agent/worker_command`, `scherzo/control/command`, and config/domain modules.

24. Define an explicit `turn_loop.Dependencies` or `Settings` record rather than passing the entire runner context as many arguments. Include session, issue id, turn number, current token totals, runtime config, update emitter, command subject, prompt queue/operator-control state, turn deadlines, stall deadlines, workspace path, and secrets. Keep it concrete and documented in the module.

25. Update `runner.gleam` so between-turn code calls `turn_loop.run_active_turn(...)`. The return value should contain the updated pi session, turn records, updated prompt queue/operator-control state, stop-after-turn flag, and token totals or a `WorkerFailure`.

26. Add or update tests. Existing `test/agent_worker_control_test.gleam` should remain the integration proof. Add one direct test in `test/agent_turn_loop_test.gleam` only if the module can be exercised without excessive fake setup; otherwise document that fake-pi integration tests cover it.

27. Run:

    grep -n "fn active_turn_loop\|fn handle_blocking_ui_policy\|fn handle_ui_response_command" src/scherzo/agent/runner.gleam
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    The grep should return no matches in `runner.gleam`. Record the milestone.

28. Create `src/scherzo/agent/run_attempt.gleam`. Move high-level runner functions and helpers from `runner.gleam`: `run_attempt`, `run_attempt_with_commands`, `run_attempt_with_command_ready`, `run_prompt_in_workspace`, `run_prepared`, `run_pi_loop`, `loop_turns`, `finish_after_turn`, `decide_after_refresh`, `finish_success`, cleanup/failure helpers, final classification, token addition, workspace prep calls, template rendering, and compatibility probe integration.

29. Keep `runner.gleam` as a small facade that imports `agent/run_attempt.gleam` and `agent/types.gleam`, re-exposes the public functions, and preserves type names if needed during migration. The facade should not contain pi turn-loop logic, JSON decoding, port IO, workspace prep implementation, or UI policy implementation.

30. Update production callers where practical: `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/service.gleam`, and `src/scherzo/workflow_run.gleam` can import `scherzo/agent/run_attempt` for functions and `scherzo/agent/types` for result/update types. Keep test migration in the same or next green slice.

31. Run:

    grep -n "fn run_prepared\|fn run_pi_loop\|fn loop_turns\|fn finish_after_turn\|fn handle_between_turn_commands" src/scherzo/agent/runner.gleam
    wc -l src/scherzo/agent/runner.gleam src/scherzo/agent/pi_rpc.gleam src/scherzo/pi/protocol.gleam src/scherzo/pi/client.gleam src/scherzo/agent/turn_loop.gleam src/scherzo/agent/run_attempt.gleam
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    `runner.gleam` should be a small facade. Record the milestone.

32. Migrate remaining imports away from `scherzo/agent/pi_rpc` and `scherzo/agent/runner` where possible. `src/scherzo/agent/probe.gleam` should import `scherzo/pi/client`; `src/scherzo/result_artifact.gleam` should import `scherzo/pi/protocol` if it only needs record decoding or types; tests should import `pi/protocol`, `pi/client`, `agent/types`, and `agent/run_attempt` directly.

33. Decide whether to delete `src/scherzo/agent/pi_rpc.gleam` and `src/scherzo/agent/runner.gleam` or keep them as compatibility facades. If kept, each facade should be short, documented, and contain no implementation logic. If deleted, update all imports and record the decision in the Decision Log.

34. Run final structural checks:

    ! grep -R "^import scherzo/agent/pi_rpc" -n src --include='*.gleam'
    grep -n "port\.\|decode\.\|json\." src/scherzo/agent/pi_rpc.gleam 2>/dev/null || true
    grep -n "fn active_turn_loop\|fn handle_blocking_ui_policy\|fn record_decoder\|fn read_events_until_agent_end" src/scherzo/agent/runner.gleam src/scherzo/agent/pi_rpc.gleam 2>/dev/null || true
    wc -l src/scherzo/agent/runner.gleam src/scherzo/agent/pi_rpc.gleam

    Production source should not import `agent/pi_rpc`. Facades, if present, should not contain direct JSON decoder, port IO, or turn-loop implementation details. The line counts for facade files should be dramatically lower than the original 2,039 and 1,242 lines.

35. Run final validation:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

    Update this plan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective. Record the final change with a message such as `Split pi protocol, client, and agent turn loop`.

## Testing and Falsifiability

The protocol extraction is falsified if decoding the same JSON line produces different `RpcRecord` fields. Tests must cover response ids, session ids, message deltas, extension UI request messages, assistant messages from `agent_end`, captured tool calls/results from `test/fixtures/pi_tool_events_captured.jsonl`, top-level and nested tool execution aliases, token totals, and raw JSON preservation.

The client extraction is falsified if fake-pi process behavior changes. Tests must cover launch, set session name, auto retry, get state, prompt, interleaved response records, abort, extension UI cancel/value, get session stats, malformed JSON, read timeout, turn timeout, stall timeout, short read timeouts until events arrive, and diagnostics on process exit.

The operator-control extraction is falsified if worker command behavior changes. Tests must cover abort during active turn, prompt queueing during active turn and sending next turn, prompt queue full, oversized prompt rejection, stop-after-current-turn, UI policy cancel/fail/ignore/operator, UI response with pending request, UI response with no pending request, UI response too large, UI timeout, and dropped prompts on abort/cleanup.

The run-attempt extraction is falsified if existing `test/agent_runner_test.gleam`, `test/agent_worker_control_test.gleam`, daemon tests, workflow run tests, handoff tests, and result artifact tests fail or need weaker assertions. Do not weaken tests to make extraction pass.

The structural claim is falsified if `pi/protocol.gleam` imports `scherzo/port`, if `pi/client.gleam` contains JSON decoder implementation details beyond calling protocol functions, if `runner.gleam` still contains `active_turn_loop` or blocking UI policy functions, or if `pi_rpc.gleam` still contains read loops and decoder internals.

## Validation and Acceptance

Acceptance requires these commands from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Both must exit zero. The test command must report `no failures`. Expected fake-pi tests should still exercise real subprocess IO.

Structural acceptance requires:

    ! grep -R "^import scherzo/agent/pi_rpc" -n src --include='*.gleam'
    ! grep -n "fn active_turn_loop\|fn handle_blocking_ui_policy" src/scherzo/agent/runner.gleam
    ! grep -n "fn record_decoder\|type MessageObject\|fn read_events_until_agent_end" src/scherzo/agent/pi_rpc.gleam 2>/dev/null || true

If `agent/pi_rpc.gleam` or `agent/runner.gleam` remain, they must be compatibility facades only. If the project owner prefers deletion, delete them and make the first and third checks naturally pass because the files do not exist.

Behavior acceptance requires no change to pi JSON command strings, decoded fake-pi events, token totals, tool extraction, workspace hook execution, operator command replies, UI policy behavior, final classification, or cleanup behavior.

## Rollout, Recovery, and Idempotence

This is an internal refactor with no data migration. Roll out as a normal code change after tests pass. Each milestone should be green and separately described so it can be reverted independently.

If the client extraction causes timeout regressions, revert the `pi/client.gleam` milestone and keep the protocol extraction if it is green. If the turn-loop extraction causes operator-control regressions, revert only that milestone and keep protocol/client/type extractions. Do not leave duplicated active turn implementations in production code across a commit.

The steps are safe to repeat. Running format, tests, and grep checks multiple times is safe. Test temporary files should stay under `test/tmp/...`, matching existing convention.

## Artifacts and Notes

Current file sizes at plan authoring:

    2039 src/scherzo/agent/runner.gleam
    1242 src/scherzo/agent/pi_rpc.gleam

Current `pi_rpc.gleam` public surface includes `Session`, `RpcRecord`, command encoders, `decode_record`, `launch`, `prompt`, `prompt_with_ui_policy`, `send_prompt`, `read_turn_record`, `send_abort`, UI response helpers, `get_session_stats`, and `terminate`.

Current `runner.gleam` public surface includes `FinalClassification`, `WorkerSuccess`, `WorkerFailure`, `PiUpdate`, `run_attempt`, `run_attempt_with_commands`, `run_attempt_with_command_ready`, and `run_prompt_in_workspace`.

Keep the fake-pi fixture `test/fixtures/fake_pi_rpc.sh` as the integration proof. Do not replace all fake-pi tests with pure unit tests; the port/process boundary is exactly the risky behavior this plan must continue to exercise.

## Interfaces and Dependencies

In `src/scherzo/pi/protocol.gleam`, define or move:

    pub type RpcRecord { ...same fields as current pi_rpc.RpcRecord... }
    pub type Data { ...same fields as current pi_rpc.Data... }

    pub fn encode_set_session_name(id: String, name: String) -> String
    pub fn encode_set_auto_retry(id: String, enabled: Bool) -> String
    pub fn encode_get_state(id: String) -> String
    pub fn encode_prompt(id: String, message: String) -> String
    pub fn encode_abort(id: String) -> String
    pub fn encode_get_session_stats(id: String) -> String
    pub fn encode_extension_ui_response(id: String) -> String
    pub fn encode_extension_ui_value_response(id: String, value: String) -> String
    pub fn decode_record(line: String) -> Result(RpcRecord, error.PiRpcError)

This module may import `gleam/json`, `gleam/dynamic/decode`, `gleam/list`, `gleam/option`, `gleam/string`, `scherzo/domain`, and `scherzo/error`. It must not import `scherzo/port`, `gleam/erlang/process`, `scherzo/agent/worker_command`, `scherzo/tracker`, `scherzo/template`, or workspace modules.

In `src/scherzo/pi/client.gleam`, define or move:

    pub type Session { ...same fields as current pi_rpc.Session... }

    pub fn launch(command: String, cwd: String, session_name: String, auto_retry: Bool, read_timeout_ms: Int) -> Result(Session, error.PiRpcError)
    pub fn send_prompt(session: Session, message: String, read_timeout_ms: Int) -> Result(#(Session, List(protocol.RpcRecord)), error.PiRpcError)
    pub fn read_turn_record(session: Session, read_timeout_ms: Int, turn_deadline_ms: Int, stall_deadline_ms: Int) -> Result(#(Session, Option(protocol.RpcRecord)), error.PiRpcError)
    pub fn send_abort(session: Session, read_timeout_ms: Int) -> Result(#(Session, List(protocol.RpcRecord)), error.PiRpcError)
    pub fn send_extension_ui_cancel(session: Session, request_id: String, read_timeout_ms: Int) -> Result(#(Session, List(protocol.RpcRecord)), error.PiRpcError)
    pub fn send_extension_ui_value(session: Session, request_id: String, value: String, read_timeout_ms: Int) -> Result(#(Session, List(protocol.RpcRecord)), error.PiRpcError)
    pub fn get_session_stats(session: Session, read_timeout_ms: Int) -> Result(#(Session, domain.TokenTotals), error.PiRpcError)
    pub fn terminate(session: Session) -> Result(Nil, error.PiRpcError)

This module may import `scherzo/pi/protocol`, `scherzo/port`, `scherzo/error`, `scherzo/domain`, and stdlib modules. It must not import `scherzo/agent/runner`, `scherzo/agent/turn_loop`, `scherzo/template`, `scherzo/workspace`, or tracker modules.

In `src/scherzo/agent/types.gleam`, define the public agent result/update types currently in `runner.gleam`.

In `src/scherzo/agent/operator_control.gleam`, define pending UI and prompt queue state plus command-decision helpers. This module may import `scherzo/agent/worker_command` and `scherzo/control/command`; it should not import pi client, tracker, template, workspace, or handoff modules.

In `src/scherzo/agent/turn_loop.gleam`, expose a function such as:

    pub fn run_active_turn(context: Context) -> Result(TurnResult, types.WorkerFailure)

The exact context fields may be a record, but they must be explicit and should not require importing `runner.gleam`.

In `src/scherzo/agent/run_attempt.gleam`, expose:

    pub fn run_attempt(...same arguments as current runner.run_attempt...) -> Result(types.WorkerSuccess, types.WorkerFailure)
    pub fn run_attempt_with_commands(...same arguments as current runner.run_attempt_with_commands...) -> Result(types.WorkerSuccess, types.WorkerFailure)
    pub fn run_attempt_with_command_ready(...same arguments as current runner.run_attempt_with_command_ready...) -> Result(types.WorkerSuccess, types.WorkerFailure)
    pub fn run_prompt_in_workspace(...same arguments as current runner.run_prompt_in_workspace...) -> Result(types.WorkerSuccess, types.WorkerFailure)

The old `src/scherzo/agent/runner.gleam` facade, if kept, should forward to these functions and re-expose types from `agent/types.gleam` only.
