# Separate pi protocol, pi client IO, and agent turn-loop orchestration

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, the riskiest part of Scherzo's agent execution path should be easier to reason about and test. JSON protocol encoding/decoding for pi RPC should live in a protocol module with no process IO. Port launch, command sending, stdout reading, deadlines, and termination should live in a pi client module. Operator controls and active turn state should live in agent-specific modules. The existing high-level behavior should remain the same: Scherzo still prepares workspaces, renders prompts, optionally probes pi compatibility, starts pi, runs turns, handles operator prompts and blocking UI requests, accounts for tokens, refreshes issue state, classifies the final issue, and cleans up.

The visible proof is that `src/scherzo/agent/pi_rpc.gleam` no longer mixes JSON protocol logic with port/process management and turn event loops, and `src/scherzo/agent/runner.gleam` no longer contains the active turn loop, UI policy handling, operator command queueing, and high-level run composition in one 2,046-line file. From the repository root, `direnv exec . gleam test` must continue to pass; at the 2026-05-03 review baseline it reported `556 passed, no failures`. Existing fake-pi tests must still prove launch, prompt, abort, UI response, tool-event extraction, timeout, and stall behavior.

## Problem Framing and Constraints

The current `src/scherzo/agent/runner.gleam` is 2,046 lines as of the 2026-05-03 review. It defines public worker result/update types, prepares workspaces, renders templates, runs compatibility probes, launches pi, runs a multi-turn loop, handles worker commands from operators, implements blocking UI policy, accounts for tokens, refreshes tracker state, classifies final issues, and performs cleanup. The active turn loop starts around `active_turn_loop` and command/UI handling spans functions such as `handle_active_command`, `handle_turn_record`, `handle_extension_ui_record`, `handle_blocking_ui_policy`, `handle_ui_response_command`, `handle_operator_ui_timeout`, and `handle_abort_command`.

The current `src/scherzo/agent/pi_rpc.gleam` is 1,270 lines. It defines `Session`, a large loose `RpcRecord`, command JSON encoders, response decoders, process launch, prompt sending, read loops, timeout and stall handling, blocking UI request handling, token and tool extraction, port error mapping, and termination. `RpcRecord` is a wide record of optional fields because it tries to represent responses, lifecycle events, messages, tool calls, tool results, UI requests, session stats, and raw JSON in one type. `launch` is in the same module as JSON decoding. `read_events_until_agent_end` and `handle_blocking_ui_request` live beside protocol decoders.

This matters because pi protocol handling combines blocking IO, timeouts, external process failure, cancellation, operator intervention, and lossy JSON payloads. When protocol decoding, client transport, and agent turn policy are braided together, it is hard to test one concern without starting a fake process and hard to change one behavior without touching unrelated code.

The plan must preserve current runtime behavior and public daemon behavior. It must not change the pi wire protocol, fake-pi fixture expectations, EventHub update shape, worker command protocol, workspace preparation semantics, cleanup policy, or final issue classification. It may introduce compatibility facades during migration, but final production code should import the owner modules directly.

## Strategy Overview

Use a strangler refactor. First move pure protocol code out of `pi_rpc.gleam`; this is low risk because it has no IO. Then move only stepwise port/client code into `src/scherzo/pi/client.gleam`, leaving a temporary `src/scherzo/agent/pi_rpc.gleam` facade while tests and callers migrate. Do not move the legacy high-level `prompt` or `prompt_with_ui_policy` helpers into `pi/client.gleam`; they combine client IO with UI policy and should either remain in a temporary agent-level facade or be replaced by test-local helpers before final cleanup. Next move public agent result/update types into `src/scherzo/agent/types.gleam`, but migrate qualified type constructors and variants rather than relying on Gleam type aliases to preserve `runner.WorkerSuccess(...)` or `runner.FinalTerminal`. Then extract operator-control state and active turn-loop behavior from `runner.gleam`. Finally move high-level run composition into `src/scherzo/agent/run_attempt.gleam`, leaving `runner.gleam` either deleted or reduced to a small compatibility wrapper for functions only.

The desired final ownership is:

- `src/scherzo/pi/protocol.gleam`: encode/decode only. It should know JSON and pi record shapes, but it should not import `scherzo/port`, launch processes, read stdout, or implement operator policy.
- `src/scherzo/pi/client.gleam`: launch/send/read/terminate only. It should know the port abstraction, deadlines, timeout mapping, and protocol module, but it should not know Scherzo workspaces, tracker issues, template rendering, handoff, final classification, or UI-request policy.
- `src/scherzo/agent/operator_control.gleam`: prompt queue, stop-after-turn flag, abort/queue/UI response command decisions, size guards, pending UI state, and the reply/update effects that the caller must perform.
- `src/scherzo/agent/update.gleam`: construction and redaction of `types.PiUpdate` values from lifecycle events and `protocol.RpcRecord` values. It owns raw JSON redaction, operator prompt redaction, tool text truncation, and token update constructors so `turn_loop.gleam` and `run_attempt.gleam` do not duplicate update policy or import each other.
- `src/scherzo/agent/turn_loop.gleam`: the active turn finite-state machine. It reads records from `pi/client`, applies operator-control decisions, emits updates, and returns a turn result or cleanup request. It may send pi turn-local commands such as abort or extension UI responses, but it must not perform workspace cleanup, tracker refresh, handoff, or final classification.
- `src/scherzo/agent/run_attempt.gleam`: high-level composition: workspace prep, prompt render, optional probe, pi launch, turn loop iteration, state refresh, classification, worker-failure construction, and cleanup.
- `src/scherzo/agent/types.gleam`: shared result/update types used by daemon, workflow run, handoff, result artifacts, event publishing, step artifacts, and tests.

## Alternatives Considered

One alternative is to add comments or section headers to `runner.gleam` and `pi_rpc.gleam`. That does not reduce coupling or make protocol decoding testable without port IO.

Another alternative is a big-bang rewrite of the agent runner and pi client. That is too risky. The fake pi tests cover many edge cases, and a broad rewrite would make regressions difficult to isolate.

A third alternative is to keep all public names in `agent/pi_rpc.gleam` and only move private helpers. That reduces file size but preserves the misleading module boundary. This plan allows a temporary facade for migration, but final production code should import `scherzo/pi/protocol` and `scherzo/pi/client` directly.

A fourth alternative is to introduce an OTP actor for the pi client immediately. That may be useful later, but the current problem is module ownership. A process boundary would add ordering and supervision concerns before the simpler protocol/client/turn-loop split is proven.

## Risks and Countermeasures

The main behavior risk is changing timeout, stall, or interleaved-response semantics. Countermeasure: keep the existing fake-pi timeout tests in `test/pi_rpc_test.gleam` or their migrated equivalents, and add focused client tests before moving code. Do not change deadline calculations, skipped-record limits, skipped-record byte accounting, or the order in which command responses and event records are surfaced while extracting modules.

The main protocol risk is changing decoded `RpcRecord` fields such as tool name, tool input, tool output, tool status, assistant messages, tokens, or raw JSON. Countermeasure: move protocol tests first and keep assertions unchanged for captured fixtures such as `test/fixtures/pi_tool_events_captured.jsonl`. Keep `RpcRecord.type_` as the raw pi event string in `pi/protocol.gleam`; convert to `src/scherzo/agent/pi_event.gleam` only in agent-level update code so the lower `pi` package does not depend on `scherzo/agent`.

The main update-surface risk is changing what operators and event subscribers see even if the underlying protocol records are unchanged. Countermeasure: move `PiUpdate` construction into `src/scherzo/agent/update.gleam` as its own small slice, keep raw JSON redaction, operator prompt redaction, token totals, tool text truncation, request id/method propagation, and `pi_event.from_string` conversion unchanged, and add direct tests for `update.from_record` plus existing agent runner/event publisher integration tests.

The main operator-control risk is changing reply timing, reply variant, command ordering, or queue semantics. Countermeasure: preserve tests in `test/agent_worker_control_test.gleam`, especially abort, queued prompt, UI response, UI timeout, and prompt-too-large behavior. Add direct unit tests for the extracted `operator_control` module that assert the exact `worker_command.Reply` variant and message for active-turn and between-turn modes.

The main compatibility risk is changing imports used by daemon, workflow run, handoff, event publishing, step artifacts, result artifacts, and tests. Countermeasure: introduce `agent/types.gleam` and temporary facade modules, but do not assume type aliases preserve qualified constructors or variants in Gleam. Search for `runner.WorkerSuccess(`, `runner.WorkerFailure(`, `runner.PiUpdate(`, `runner.FinalActive`, `runner.FinalTerminal`, and `runner.FinalNonActive`; migrate those call sites to `agent/types.gleam` in the same green slice that moves the type definitions. Keep `runner.run_attempt`, `runner.run_attempt_with_commands`, and `runner.run_attempt_with_command_ready` available until all production callers use `agent/run_attempt.gleam`.

The main ownership risk is accidentally moving legacy UI policy into the low-level pi client or workspace cleanup into the turn loop. Countermeasure: `pi/client.gleam` exposes only stepwise launch/send/read/stat/terminate primitives. `turn_loop.gleam` may emit updates and send pi turn-local commands, but `run_attempt.gleam` remains the owner of workspace hooks, cleanup, tracker refresh, final classification, and conversion to `types.WorkerFailure`.

The main import-cycle risk is letting `pi/client.gleam` import agent modules, letting `pi/protocol.gleam` import client modules, or letting shared update helpers import orchestration modules. Countermeasure: dependencies must point one way: protocol is lowest; client imports protocol; agent types import only domain/error/pi-event/session-event types; agent update imports protocol and types; agent operator control imports worker commands; agent turn loop imports client, protocol, pi events, update, and operator control; run attempt imports turn loop and update; runner facade imports run attempt.

## Progress

- [x] (2026-05-02 10:01Z) Approved the workspace `.envrc` after inspecting it because `direnv exec . gleam test` was blocked in the original review workspace.
- [x] (2026-05-03 07:00Z) Re-verified the plan-review baseline with `direnv exec . gleam test`; it passed with `556 passed, no failures`.
- [x] (2026-05-03 07:01Z) Fact-checked pre-implementation file sizes: `src/scherzo/agent/runner.gleam` had 2,046 lines and `src/scherzo/agent/pi_rpc.gleam` had 1,270 lines.
- [x] (2026-05-03 07:05Z) Reviewed current public functions, imports, constructor call sites, update-construction helpers, and tests in `pi_rpc.gleam`, `runner.gleam`, `pi_event.gleam`, `result_artifact.gleam`, `test/pi_rpc_test.gleam`, `test/agent_runner_test.gleam`, and `test/agent_worker_control_test.gleam`.
- [x] (2026-05-03 09:19Z) Milestone 0: verified the workspace started clean with `jj status --color=never`; `direnv exec . gleam test` compiled the project but failed before implementation in `test/execplan_implementation_helper_test.gleam` with `554 passed, 2 failures`.
- [x] (2026-05-03 09:25Z) Milestone 1: extracted pure JSON protocol records, encoders, decoders, token decoding, assistant-message extraction, and tool-event extraction to `src/scherzo/pi/protocol.gleam`; added `test/pi_protocol_test.gleam` and reduced `src/scherzo/agent/pi_rpc.gleam` to protocol/client forwarding.
- [x] (2026-05-03 09:26Z) Milestone 2: extracted launch, command send, response collection, turn-record reading, deadlines, timeout mapping, stats, and termination to `src/scherzo/pi/client.gleam`; moved fake-pi client characterization to `test/pi_client_test.gleam` and kept `pi/client.gleam` free of `UiRequestPolicy` and `prompt_with_ui_policy`.
- [x] (2026-05-03 09:27Z) Milestone 3: moved `FinalClassification`, `WorkerSuccess`, `WorkerFailure`, and `PiUpdate` to `src/scherzo/agent/types.gleam`; migrated production and test constructor/variant references from `runner.*` to `agent_types.*`.
- [x] (2026-05-03 09:29Z) Milestone 4: added `src/scherzo/agent/operator_control.gleam` with pure prompt queue, stop, abort, pending UI, size guard, and explicit effect decisions; added `test/agent_operator_control_test.gleam` for exact reply/effect behavior.
- [x] (2026-05-03 09:32Z) Milestone 5: extracted the active turn loop, blocking UI policy handling, UI response send decisions, operator UI timeout handling, and turn-record update construction to `src/scherzo/agent/turn_loop.gleam`; `runner.gleam` no longer contains `active_turn_loop`, `handle_blocking_ui_policy`, or `handle_ui_response_command`.
- [x] (2026-05-03 09:35Z) Milestone 6: moved high-level workspace preparation, prompt rendering, probe integration, launch, multi-turn composition, tracker refresh, classification, cleanup, and failure construction to `src/scherzo/agent/run_attempt.gleam`; reduced `src/scherzo/agent/runner.gleam` to a 101-line function/type facade.
- [x] (2026-05-03 09:37Z) Milestone 7: migrated production imports away from `scherzo/agent/pi_rpc`, kept short compatibility facades for `agent/pi_rpc.gleam` and `agent/runner.gleam`, ran final structural checks, and recorded the retrospective.
- [x] (2026-05-03 09:59Z) Review feedback: wired active-turn and between-turn runtime command handling through `operator_control.handle_command`, interpreted its reply, prompt, abort, stop, and UI-send effects in `src/scherzo/agent/turn_loop.gleam` and `src/scherzo/agent/run_attempt.gleam`, and reran validation with the same two pre-existing publish-helper failures.

## Surprises & Discoveries

- Observation: Current production code does not construct `pi_rpc.RpcRecord(...)` or `pi_rpc.Session(...)` directly.
  Evidence: searches for `RpcRecord(` and `pi_rpc.Session(` in `src` and `test` returned no matches. This makes temporary type aliases in a facade feasible because callers use field access and function returns, not constructors.

- Observation: `runner.gleam` already uses a stepwise pi RPC API for the active command-aware loop.
  Evidence: production runner calls `pi_rpc.launch`, `pi_rpc.send_prompt`, and `pi_rpc.read_turn_record`; the higher-level `pi_rpc.prompt` and `prompt_with_ui_policy` helpers are used by tests, not by the runner.

- Observation: The existing tests are well positioned for this refactor.
  Evidence: `test/pi_rpc_test.gleam` already separates codec assertions, stepwise prompt reads, deadlines, abort/UI helper commands, launch/prompt/stats, tool event decoding, malformed JSON, and timeout/stall scenarios. `test/agent_worker_control_test.gleam` already exercises command-aware runner behavior.

- Observation: `src/scherzo/agent/pi_event.gleam` already exists and `runner.PiUpdate.event` is already typed as `pi_event.PiEvent`.
  Evidence: `src/scherzo/agent/pi_event.gleam` defines `PiEvent`, `from_string`, `to_string`, and `is_blocking_ui_request`; `src/scherzo/agent/runner.gleam` converts `RpcRecord.type_` with `pi_event.from_string` when creating updates. The protocol extraction should not reintroduce a dependency from `src/scherzo/pi/protocol.gleam` back to `src/scherzo/agent/pi_event.gleam`.

- Observation: Many production and test modules construct or pattern-match `runner` type constructors and variants directly.
  Evidence: searches for `runner.WorkerSuccess(`, `runner.WorkerFailure(`, `runner.PiUpdate(`, and `runner.FinalTerminal` returned matches in `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/handoff.gleam`, `src/scherzo/handoff_format.gleam`, `src/scherzo/step_artifact.gleam`, and many tests. Milestone 3 must migrate constructor call sites to `agent/types.gleam`; a simple type alias in `runner.gleam` is not a safe compatibility plan for qualified constructors.

- Observation: The 2026-05-03 review baseline drifted since the prior review.
  Evidence: `wc -l src/scherzo/agent/runner.gleam src/scherzo/agent/pi_rpc.gleam` reported 2,046 and 1,270 lines, and `direnv exec . gleam test` reported `556 passed, no failures`.

- Observation: `runner.gleam` contained shared update-construction behavior that otherwise had no clear owner after the turn-loop split.
  Evidence: `lifecycle_update`, `lifecycle_update_with_message`, `lifecycle_update_with_request`, `pi_session_started_update`, `token_update`, `update_from_record`, `redact_operator_message`, `redact_message`, and `normalize_tool_text` were used across probe/run-attempt flow, between-turn operator handling, active-turn records, UI policy, abort handling, and dropped-prompt emission.

- Observation: The implementation workspace did not match the older green baseline before implementation began.
  Evidence: the first `direnv exec . gleam test` run on 2026-05-03 compiled successfully but failed two publish-helper assertions in `test/execplan_implementation_helper_test.gleam`, reporting `554 passed, 2 failures`; the same two tests were still the only failures after the refactor, with `562 passed, 2 failures` after added characterization coverage.

- Observation: The old test-only `pi_rpc.prompt` helper mixed a fire-and-forget UI cancel path with process IO, which did not fit the low-level `pi/client.gleam` stepwise API.
  Evidence: `test/fixtures/fake_pi_rpc.sh` only responds to `extension_ui_response` inside prompt handling when `FAKE_PI_UI_DIALOG_WAITS=1` is set. The extracted client keeps response-collecting UI command helpers, and the old high-level prompt tests were rewritten to stepwise client tests plus a runner-level cancel policy test.

## Decision Log

- Decision: Keep a temporary `src/scherzo/agent/pi_rpc.gleam` facade while migrating callers.
  Rationale: Existing tests and modules import `scherzo/agent/pi_rpc`. A facade lets protocol and client extraction land in small green commits. Final production imports should move to `scherzo/pi/protocol` and `scherzo/pi/client` before completion.
  Date: 2026-04-30

- Decision: Move shared worker result/update types to `src/scherzo/agent/types.gleam` before extracting turn loop and run attempt, and migrate qualified constructor/variant call sites in the same green slice.
  Rationale: Daemon, workflow run, handoff, handoff formatting, event publishing, step artifacts, and tests use `runner.WorkerSuccess`, `runner.WorkerFailure`, `runner.PiUpdate`, and final-classification variants. A neutral types module avoids import cycles when `turn_loop` and `run_attempt` are split, but relying on aliases would leave qualified constructors ambiguous or broken.
  Date: 2026-05-02

- Decision: Extract `src/scherzo/agent/update.gleam` as the owner of `types.PiUpdate` constructors and redaction/truncation policy.
  Rationale: Both the high-level run attempt and active turn loop need to emit lifecycle, token, operator-prompt, UI-request, and raw-record updates. If those helpers remain in `runner.gleam`, the facade stays too large; if they move into `run_attempt.gleam`, `turn_loop.gleam` must import upward; if they are duplicated, raw JSON redaction and tool text truncation can drift. A small update module keeps the dependency direction acyclic and makes update behavior directly testable.
  Date: 2026-05-03

- Decision: Do not introduce a pi client actor in this plan.
  Rationale: The immediate problem is module entanglement. A new process boundary would add supervision, mailbox ordering, and shutdown semantics before the simpler split is proven.
  Date: 2026-04-30

- Decision: Preserve current `RpcRecord` shape for the first protocol extraction.
  Rationale: The wide optional record is a real smell, but changing it while moving modules would combine protocol behavior changes with extraction. First move it unchanged, then optionally split response/event/tool records in a later plan or final cleanup milestone if tests make it low risk.
  Date: 2026-04-30

- Decision: Keep `src/scherzo/pi/client.gleam` free of legacy high-level prompt/UI policy helpers.
  Rationale: `prompt` and `prompt_with_ui_policy` combine process IO with blocking UI policy and are not used by production runner code. Moving them into the low-level client would recreate the coupling this plan is meant to remove. Migrate those tests to stepwise client helpers, a temporary agent-level compatibility helper, or turn-loop integration tests instead.
  Date: 2026-05-02

- Decision: Keep workspace cleanup and worker-failure construction in `src/scherzo/agent/run_attempt.gleam`, not in `src/scherzo/agent/turn_loop.gleam`.
  Rationale: The active turn loop owns reading pi records, processing operator commands during a turn, sending turn-local pi commands, and returning turn outcomes. Workspace hooks, tracker refresh, final classification, and cleanup are high-level run-attempt responsibilities and would create a larger blast radius if moved into the turn loop.
  Date: 2026-05-02

- Decision: Treat `src/scherzo/agent/pi_event.gleam` as an agent/update-layer type during this extraction.
  Rationale: The current repository already uses typed pi update names, but `src/scherzo/pi/protocol.gleam` should remain a protocol decoder with raw wire strings to avoid a lower-level pi module importing the higher-level agent package. The conversion from raw record `type_` to `pi_event.PiEvent` belongs in `src/scherzo/agent/update.gleam`.
  Date: 2026-05-02

- Decision: Keep `src/scherzo/agent/pi_rpc.gleam` and `src/scherzo/agent/runner.gleam` as short compatibility facades rather than deleting them in this slice.
  Rationale: Tests still exercise the old import paths, and `runner` remains a useful compatibility surface for existing test helpers. Production source no longer imports `agent/pi_rpc`; `runner.gleam` now forwards functions and type aliases only and no longer exposes result/update constructors.
  Date: 2026-05-03

- Decision: Use callbacks from `run_attempt.gleam` into `turn_loop.gleam` for cleanup and abort failure construction.
  Rationale: The active turn loop needs to request cleanup on pi failures and operator aborts, but workspace hooks and `types.WorkerFailure` construction remain high-level run-attempt responsibilities. Passing explicit callbacks lets `turn_loop.gleam` avoid importing `scherzo/workspace`, tracker modules, template modules, or `runner.gleam`.
  Date: 2026-05-03

- Decision: Interpret `operator_control.handle_command` in both production command paths rather than duplicating queue, stop, rejection, and UI-response decisions in `run_attempt.gleam` and `turn_loop.gleam`.
  Rationale: Review noted that pure operator-control unit tests were weaker if runtime code only used the helper predicates. The runtime now uses the same decision API that the unit tests cover, while the interpreters still own side effects such as process replies, pi abort/UI sends, update emission, and workspace cleanup. The active-turn UI-send interpreter preserves the old behavior of keeping the pending UI request when sending the pi response fails.
  Date: 2026-05-03

- Decision: Do not create the milestone `jj describe` commits named in the original plan during this Scherzo workflow run.
  Rationale: The workflow contract for LIV-21 explicitly says not to create jj/git commits because the publish step creates the final logical jj commit after validation and review. All implementation state is instead recorded in this living ExecPlan and the working copy.
  Date: 2026-05-03

## Outcomes & Retrospective

The decomposition is implemented. Protocol encoding and decoding now live in `src/scherzo/pi/protocol.gleam`; process IO, deadlines, response collection, stats, and termination live in `src/scherzo/pi/client.gleam`; shared worker result/update types live in `src/scherzo/agent/types.gleam`; pure operator-control decisions live in `src/scherzo/agent/operator_control.gleam`; active turn orchestration lives in `src/scherzo/agent/turn_loop.gleam`; and high-level run-attempt composition lives in `src/scherzo/agent/run_attempt.gleam`. The old `src/scherzo/agent/pi_rpc.gleam` and `src/scherzo/agent/runner.gleam` files remain as short compatibility facades with 119 and 101 lines respectively, down from the reviewed 1,242 and 2,046 line files.

Review feedback has been applied so production command handling now calls `operator_control.handle_command` during both active turns and between turns. `src/scherzo/agent/turn_loop.gleam` and `src/scherzo/agent/run_attempt.gleam` interpret the returned effects, which keeps side effects in the orchestration layers while making the pure operator-control tests exercise the same decision API used at runtime.

The main intended structural boundaries are now enforceable by grep checks: production source has no `import scherzo/agent/pi_rpc`; the pi RPC facade has no JSON decoder or port IO implementation; the runner facade has no active turn loop or high-level run composition; and `src/scherzo/pi/client.gleam` has no legacy UI policy helper, workspace, tracker, or runner imports. Characterization coverage was split into protocol and client test modules, with an added runner-level UI cancel policy test and direct operator-control unit tests. Full validation is not green in this workspace because two `execplan_implementation_helper_test` publish-helper assertions failed before implementation and still fail after review-feedback fixes; this should be treated as a pre-existing validation issue unless later evidence connects it to this refactor.

## Context and Orientation

Scherzo runs pi through an RPC-style subprocess. After this implementation, pure wire-protocol JSON lives in `src/scherzo/pi/protocol.gleam`: it defines `protocol.RpcRecord`, command encoders, and record decoders. Process IO lives in `src/scherzo/pi/client.gleam`: it starts a `scherzo/port` process, writes JSON command lines to stdin, reads JSONL records from stdout, maps port errors to `error.PiRpcError`, accounts for deadlines, gets session stats, and terminates the process. The old `src/scherzo/agent/pi_rpc.gleam` file is now a short compatibility facade that forwards protocol and client functions.

The current high-level agent run implementation is `src/scherzo/agent/run_attempt.gleam`. It owns workspace preparation, prompt rendering, optional compatibility probing, pi launch, multi-turn composition, tracker refresh, final classification, cleanup, and conversion to `types.WorkerSuccess` or `types.WorkerFailure`. `src/scherzo/agent/runner.gleam` is now a short function/type-alias facade for the old public run-function names and type annotation paths. Shared agent result and update types live in `src/scherzo/agent/types.gleam`, and constructor/variant call sites use `agent_types.WorkerSuccess`, `agent_types.WorkerFailure`, `agent_types.PiUpdate`, and `agent_types.FinalTerminal` rather than `runner`-qualified constructors.

Worker commands are defined in `src/scherzo/agent/worker_command.gleam`. Pure command decisions and pending UI state live in `src/scherzo/agent/operator_control.gleam`; the active turn finite-state machine lives in `src/scherzo/agent/turn_loop.gleam`. Operator commands can abort, request stop after current turn, queue another prompt, or respond to a pending UI request, and `turn_loop.gleam` calls back into `run_attempt.gleam` for cleanup/failure construction.

Pi RPC fake-process coverage comes from `test/fixtures/fake_pi_rpc.sh` and tests under `test/pi_protocol_test.gleam`, `test/pi_client_test.gleam`, `test/pi_rpc_test.gleam`, `test/agent_operator_control_test.gleam`, `test/agent_runner_test.gleam`, and `test/agent_worker_control_test.gleam`. These tests are the safety net for this refactor.

## Preconditions and Verified Facts

Before implementation starts, run from the repository root:

    jj status
    direnv exec . gleam test
    wc -l src/scherzo/agent/runner.gleam src/scherzo/agent/pi_rpc.gleam

If `direnv exec . gleam test` reports that `.envrc` is blocked, inspect `.envrc`; at review time it contained only devenv setup plus optional `.env`/`.env.local` loading. Then run `direnv allow .` from the repository root and retry the test command.

At the 2026-05-03 plan review, the file-size output was:

    2046 src/scherzo/agent/runner.gleam
    1270 src/scherzo/agent/pi_rpc.gleam
    3316 total

The baseline test output at the same review was:

    556 passed, no failures

Use these searches to normalize if the tree has changed:

    grep -R "import scherzo/agent/pi_rpc" -n src test --include='*.gleam'
    grep -R "import scherzo/agent/runner" -n src test --include='*.gleam'
    grep -R "runner\.WorkerSuccess\|runner\.WorkerFailure\|runner\.PiUpdate\|runner\.Final" -n src test --include='*.gleam'
    grep -n "^pub type \|^type \|^pub fn \|^fn " src/scherzo/agent/pi_rpc.gleam
    grep -n "^pub type \|^type \|^pub fn \|^fn " src/scherzo/agent/runner.gleam

At review time, production imports of `agent/pi_rpc` were `src/scherzo/agent/probe.gleam`, `src/scherzo/agent/runner.gleam`, and `src/scherzo/result_artifact.gleam`; tests also import it. Production imports of `agent/runner` were `src/scherzo/step_artifact.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/event_publisher.gleam`, `src/scherzo/orchestrator/effect_runner.gleam`, `src/scherzo/handoff_format.gleam`, `src/scherzo/handoff.gleam`, and `src/scherzo/workflow_run.gleam`; tests also import it.

This repository uses Jujutsu. Do not use mutating `git` commands. Use `jj status`, `jj describe -m "message"`, and `jj new` for milestone commit discipline.

## Scope Boundaries

In scope: extracting pure pi protocol encoding/decoding; extracting pi client process IO and deadlines; migrating pi RPC tests to protocol/client modules; moving shared agent types; extracting `PiUpdate` construction and redaction policy; extracting operator prompt/UI/abort control decisions; extracting the active turn FSM; extracting high-level run-attempt composition; shrinking or removing `runner.gleam` and `pi_rpc.gleam` facades.

Out of scope: changing the pi JSON protocol; changing fake pi fixture behavior; changing worker command message variants; changing EventHub payload shape; changing token accounting semantics; changing workspace hook behavior; changing final issue classification rules; introducing supervision/OTP actors for pi; splitting the large `RpcRecord` into a richer event ADT beyond what is required for extraction.

`src/scherzo/agent/pi_event.gleam` has already introduced typed pi update names. Use `pi_event.PiEvent` in `agent/types.gleam`, `agent/turn_loop.gleam`, update construction, event publishing, and tests that deal with Scherzo updates. Keep `protocol.RpcRecord.type_` as the raw string decoded from pi JSON so `src/scherzo/pi/protocol.gleam` stays independent of `src/scherzo/agent`.

## Milestones

Milestone 0 verifies existing characterization tests, adds any missing coverage after tree drift, and records current behavior. At the end, tests prove the current protocol strings, decoded record fields, fake-pi IO, timeout/stall behavior, and operator-control behavior before any extraction.

Milestone 1 creates `src/scherzo/pi/protocol.gleam`. At the end, JSON command encoding and record decoding live there, and `agent/pi_rpc.gleam` delegates encoding/decoding to the new module.

Milestone 2 creates `src/scherzo/pi/client.gleam`. At the end, process launch, command sending, response collection, turn-record reading, timeouts, stall deadlines, session stats, and termination live there. Legacy `prompt` and `prompt_with_ui_policy` coverage has either been rewritten around stepwise client calls, kept temporarily in an agent-level facade, or migrated to turn-loop integration tests; it must not live in `pi/client.gleam`. `agent/pi_rpc.gleam` is a small compatibility facade for the remaining old import paths.

Milestone 3 creates `src/scherzo/agent/types.gleam` and `src/scherzo/agent/update.gleam`. At the end, `FinalClassification`, `WorkerSuccess`, `WorkerFailure`, and `PiUpdate` live in `types.gleam`; lifecycle/token/raw-record update constructors and redaction/truncation helpers live in `update.gleam`; direct constructor and variant call sites use `agent/types.gleam`; and `runner.gleam` no longer needs to be imported solely for types or update construction.

Milestone 4 creates `src/scherzo/agent/operator_control.gleam`. At the end, prompt queue manipulation, stop-after-turn state, pending UI state, size guards, command reply decisions, and caller-executed command effects are outside `runner.gleam`.

Milestone 5 creates `src/scherzo/agent/turn_loop.gleam`. At the end, the active turn loop and blocking UI policy handling are outside `runner.gleam`, and they depend only on pi client, protocol records, pi events, operator control, worker commands, config values, and update emission. Workspace cleanup and final failure construction remain in run-attempt code.

Milestone 6 creates `src/scherzo/agent/run_attempt.gleam`. At the end, workspace preparation, template rendering, compatibility probing, pi launch, multi-turn orchestration, state refresh, final classification, and cleanup are composed there. `runner.gleam` becomes a wrapper or is deleted after all imports move.

Milestone 7 migrates imports, removes obsolete facades if possible, runs final structural checks, and records outcomes.

## Plan of Work

Begin by moving pure protocol code because it is easiest to validate and has the lowest risk. Then move stepwise client IO while preserving the public `pi_rpc` API through a facade and avoiding a new UI-policy dependency in `pi/client.gleam`. Next move shared types so later agent modules can import them without depending on `runner.gleam`, migrating constructor and variant call sites in the same slice. In the same milestone, move `PiUpdate` constructors and redaction/truncation helpers to `agent/update.gleam` so `run_attempt.gleam` and `turn_loop.gleam` can share update policy without importing each other. Extract operator control before turn loop so the turn loop can call a smaller API instead of carrying every queue/UI/reply detail itself. Extract the active turn loop before high-level run-attempt composition, but leave workspace cleanup and worker-failure construction in the high-level run-attempt layer. Finally, reduce `runner.gleam` to a facade that calls `agent/run_attempt.gleam`, then update production imports to the new modules.

Each milestone should be behavior-preserving. When a function moves, copy it first, call it from the old place, run tests, then delete the old implementation. Avoid simultaneous semantic cleanup. The only acceptable behavior changes in this plan are test-only import changes and module ownership changes. If a compatibility helper would force a lower-level module to import a higher-level policy module, keep that helper in a temporary higher-level facade or test helper instead.

## Concrete Steps

1. From the repository root, run `jj status`. If unrelated source changes exist, stop and decide whether to move to a clean workspace or record them in this plan. If only ignored build artifacts such as `_build/` or `.direnv/` exist, leave them alone.

2. Run `direnv exec . gleam test`. Expect `no failures`. At the 2026-05-03 review baseline, the suite reported `556 passed, no failures`. If `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry.

3. Confirm the characterization tests before moving code. In `test/pi_rpc_test.gleam`, the relevant existing tests at review time were `codec_helpers_encode_commands_test`, `decode_response_and_event_test`, `decode_extension_ui_request_message_test`, `decode_agent_end_assistant_messages_test`, `decode_captured_assistant_tool_call_and_tool_result_test`, `decode_top_level_and_data_tool_execution_aliases_test`, `stepwise_prompt_read_and_stats_with_fake_pi_test`, `read_turn_record_uses_absolute_deadlines_test`, `send_abort_and_ui_response_helpers_test`, `launch_prompt_and_stats_with_fake_pi_test`, `prompt_with_fake_tool_events_surfaces_tool_records_test`, `probe_launches_without_prompt_test`, `malformed_json_and_timeout_fail_test`, `prompt_allows_short_read_timeouts_until_event_test`, `prompt_fails_when_stall_timeout_expires_test`, `prompt_fails_when_turn_timeout_expires_before_agent_end_test`, `turn_timeout_and_failed_stats_are_errors_test`, `extension_ui_fail_policy_rejects_dialog_test`, `extension_ui_ignore_policy_does_not_send_cancel_test`, `extension_ui_operator_policy_rejects_instead_of_cancelling_test`, and `extension_ui_dialog_is_cancelled_test`. If any of those tests are missing after tree drift, add an equivalent characterization test before extraction.

4. Confirm command-aware runner coverage. In `test/agent_worker_control_test.gleam`, the relevant existing tests at review time were `abort_command_stops_fake_pi_worker_test`, `operator_prompt_queued_during_turn_and_sent_next_turn_test`, `operator_ui_request_timeout_cancels_before_read_timeout_test`, and `operator_ui_request_cancel_response_test`. If the tree has drifted and one is missing, add it now. Run `direnv exec . gleam test` and record the pass count in Progress.

5. Create directory `src/scherzo/pi/` if it does not exist. Create `src/scherzo/pi/protocol.gleam`.

6. Move these protocol-only definitions from `src/scherzo/agent/pi_rpc.gleam` to `src/scherzo/pi/protocol.gleam`: `RpcRecord`, command encoders `encode_set_session_name`, `encode_set_auto_retry`, `encode_get_state`, `encode_prompt`, `encode_abort`, `encode_get_session_stats`, `encode_extension_ui_response`, `encode_extension_ui_value_response`, `decode_record`, private decoder types `MessageObject`, `AgentEndMessage`, `ContentItem`, `Data`, decoder helpers, token decoder, tool extraction helpers, and any JSON-only helper required by decoding. Keep `RpcRecord.type_` as a raw `String`; do not import `scherzo/agent/pi_event` into this protocol module.

7. In `src/scherzo/agent/pi_rpc.gleam`, import `scherzo/pi/protocol`. Replace moved implementations with forwarding functions. Use type aliases for `RpcRecord` and `Data` if needed. This is safe for `RpcRecord` and `Data` because review-time searches showed current source and tests do not construct `pi_rpc.RpcRecord(...)` or `pi_rpc.Data(...)` outside the decoder implementation.

8. Create `test/pi_protocol_test.gleam` and move protocol-only tests from `test/pi_rpc_test.gleam`: command encoder checks, decode response/event, extension UI request message, assistant messages, captured tool call/tool result, and top-level/data tool execution aliases. Update imports to `scherzo/pi/protocol`.

9. Run:

    grep -n "decode_record\|record_decoder\|encode_prompt\|MessageObject" src/scherzo/agent/pi_rpc.gleam
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    The grep may show forwarding functions in `pi_rpc.gleam`, but JSON decoder implementation details such as `record_decoder` and `MessageObject` should live only in `pi/protocol.gleam`. Record the milestone with `jj describe -m "Extract pi protocol codec"` and start a new change with `jj new` if keeping milestones separate.

10. Create `src/scherzo/pi/client.gleam`. Move `Session`, `launch`, `send_prompt`, `read_turn_record`, `send_abort`, `send_extension_ui_cancel`, `send_extension_ui_value`, `get_session_stats`, `terminate`, `send_expect_success`, `send_auto_retry`, `send_get_state`, response collection, skipped-record limits, deadline calculations, port error mapping, `try_pi`, `int_to_string`, and `monotonic_ms` from `agent/pi_rpc.gleam` into `pi/client.gleam`. The client module should import `scherzo/pi/protocol`, `scherzo/port`, `scherzo/error`, `scherzo/domain`, and low-level stdlib modules only. Do not move `prompt`, `prompt_with_ui_policy`, `read_events_until_agent_end`, or blocking UI policy helpers into `pi/client.gleam`.

11. Create `test/pi_client_test.gleam` and move stepwise client/port/fake-pi tests from `test/pi_rpc_test.gleam`: `stepwise_prompt_read_and_stats_with_fake_pi_test`, `read_turn_record_uses_absolute_deadlines_test`, `send_abort_and_ui_response_helpers_test`, `launch_prompt_and_stats_with_fake_pi_test`, `prompt_with_fake_tool_events_surfaces_tool_records_test`, `probe_launches_without_prompt_test`, `malformed_json_and_timeout_fail_test`, `prompt_allows_short_read_timeouts_until_event_test`, `prompt_fails_when_stall_timeout_expires_test`, `prompt_fails_when_turn_timeout_expires_before_agent_end_test`, `turn_timeout_and_failed_stats_are_errors_test`, and any diagnostics-on-process-exit test that exists after tree drift. For existing tests that call high-level `pi_rpc.prompt` or `pi_rpc.prompt_with_ui_policy`, either rewrite them to use a small test-local helper that calls `client.send_prompt` plus repeated `client.read_turn_record`, or migrate their UI-policy assertion to `test/agent_worker_control_test.gleam` or `test/agent_turn_loop_test.gleam`. Do not add public `prompt` or `prompt_with_ui_policy` functions to `pi/client.gleam`.

12. In `src/scherzo/agent/pi_rpc.gleam`, replace stepwise client implementations with forwarding functions to `pi/client.gleam`. Use `pub type Session = client.Session` as a temporary alias. If the old public `prompt` or `prompt_with_ui_policy` names must remain temporarily for tests, put their implementation in a temporary `src/scherzo/agent/pi_rpc_legacy.gleam` using `pi/client` stepwise primitives and have `agent/pi_rpc.gleam` forward to it. Mark that module as legacy test compatibility and remove it before final structural acceptance.

13. Run:

    ! grep -n "port\.\|read_turn_line\|read_until_response" src/scherzo/agent/pi_rpc.gleam
    ! grep -n "prompt_with_ui_policy\|UiRequestPolicy" src/scherzo/pi/client.gleam
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    Both grep checks should produce no matches and exit zero because of the leading `!`. `agent/pi_rpc.gleam` should have no direct `port.` calls or low-level read-loop implementations. `pi/client.gleam` should not mention `UiRequestPolicy` or `prompt_with_ui_policy`. Record the milestone with `jj describe -m "Extract pi client IO"`.

14. Create `src/scherzo/agent/types.gleam`. Move `FinalClassification`, `WorkerSuccess`, `WorkerFailure`, and `PiUpdate` from `runner.gleam` into it. Import `gleam/option.{type Option}`, `scherzo/agent/pi_event`, `scherzo/domain`, `scherzo/error`, and `scherzo/session/event as session_event`. Then create `src/scherzo/agent/update.gleam` and move `max_tool_text_chars`, `tool_text_truncated_suffix`, `lifecycle_update`, `lifecycle_update_with_message`, `lifecycle_update_with_request`, `pi_session_started_update`, `token_update`, `update_from_record`, `redact_message`, `normalize_tool_text`, `redact_operator_message`, and `emit_records` from `runner.gleam` into it, renaming the public functions to the names specified in the Interfaces and Dependencies section. In `runner.gleam`, import `scherzo/agent/types` and `scherzo/agent/update`; update internal unqualified constructors to the qualified names `types.WorkerSuccess`, `types.WorkerFailure`, `types.PiUpdate`, `types.FinalActive`, `types.FinalTerminal`, and `types.FinalNonActive`; and replace direct helper calls with `update.lifecycle`, `update.lifecycle_with_message`, `update.lifecycle_with_request`, `update.pi_session_started`, `update.token`, `update.from_record`, `update.emit_records`, and `update.redact_operator_message`. Do not move `classify`, `contains`, `add_tokens`, cleanup helpers, tracker refresh, or workspace hook calls into `agent/update.gleam`.

15. Run searches and migrate constructor call sites rather than relying on type aliases:

    grep -R "runner\.WorkerSuccess\|runner\.WorkerFailure\|runner\.PiUpdate\|runner\.Final" -n src test --include='*.gleam'
    grep -R "\(^\|[^.]\)WorkerSuccess(\|\(^\|[^.]\)WorkerFailure(\|\(^\|[^.]\)PiUpdate(" -n src test --include='*.gleam'

    The first search finds stale `runner`-qualified uses. The second search finds unqualified constructors without falsely flagging `types.WorkerSuccess(...)` or `agent_types.PiUpdate(...)`. Update production modules that use result/update types to `import scherzo/agent/types as agent_types` and replace qualified constructor and variant references. At review time the production files to check were `src/scherzo/step_artifact.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/orchestrator/event_publisher.gleam`, `src/scherzo/orchestrator/effect_runner.gleam`, `src/scherzo/handoff_format.gleam`, `src/scherzo/handoff.gleam`, and `src/scherzo/workflow_run.gleam`. Update tests in the same green slice when they construct `runner.WorkerSuccess(...)`, `runner.WorkerFailure(...)`, or `runner.PiUpdate(...)`.

16. Keep `runner.gleam` function signatures returning `types.WorkerSuccess` and `types.WorkerFailure`. If a temporary runner facade remains later, it should forward functions only; do not count on it to preserve `runner.WorkerSuccess(...)` constructors after the type move.

17. Add `test/agent_update_test.gleam` for the extracted update helpers. At minimum, test `update.from_record` with a `protocol.RpcRecord` containing `type_: "message_update"`, a secret-bearing `delta`, `raw_json`, `tool_input`, and `tool_output`; assert that the returned `types.PiUpdate` has `event == pi_event.MessageUpdate`, redacted message/raw JSON/tool fields, preserved token totals, preserved request id/method/session id, and tool text truncated at 4,096 characters with `… [truncated]` when applicable. Also test `update.lifecycle_with_request` and `update.token` directly so lifecycle request fields and token totals are not only covered through the fake-pi runner tests. Then run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Record the milestone with `jj describe -m "Move shared agent result types and update helpers"`.

18. Create `src/scherzo/agent/operator_control.gleam`. Move or recreate these concepts from `runner.gleam`: `PendingUi`, prompt queue max length of 10, operator prompt max-size guard using `worker_command.max_operator_prompt_chars` (65,536 characters at review time), UI response max-size guard using `worker_command.max_operator_ui_value_chars` (65,536 characters at review time), queue prompt decision, stop-after-turn decision, pending UI response decision, abort decision result shape, and worker-command reply decisions.

19. Design the operator-control API to be pure with explicit caller-executed effects. It must distinguish active-turn and between-turn behavior because the current replies differ. A concrete shape is:

    pub type Mode {
      BetweenTurns
      ActiveTurn
    }

    pub type State {
      State(
        prompt_queue: List(String),
        stop_after_turn: Bool,
        pending_ui: Option(PendingUi),
      )
    }

    pub type Effect {
      Reply(process.Subject(worker_command.Reply), worker_command.Reply)
      EmitPromptQueued(message: String)
      AbortRequested(reply: process.Subject(worker_command.Reply))
      StopBeforeNextTurn(reply: process.Subject(worker_command.Reply))
      SendUiCancel(reply: process.Subject(worker_command.Reply), request_id: String)
      SendUiValue(reply: process.Subject(worker_command.Reply), request_id: String, value: String)
    }

    pub fn handle_command(mode: Mode, state: State, command: worker_command.Command) -> #(State, List(Effect))

    Use these names unless Gleam requires minor type qualification changes; do not leave the caller to invent a different effect model. The API must make all side effects explicit. This module may carry reply subjects as data in effects, but it must not send process messages, call pi, emit updates, touch tracker clients, run workspace cleanup, or classify final issues. Between-turn handling must continue to drain all immediately available commands with non-blocking receives before starting the next turn, while active-turn handling must continue to check for a queued command before each pi read.

20. Add `test/agent_operator_control_test.gleam`. Test exact effects and exact reply values for prompt too large, prompt queue full, prompt queued between turns (`Applied(Some("prompt accepted for next turn"))`), prompt queued during active turn (`Queued(Some("prompt queued for next turn"))`), stop before next turn (`Applied(Some("stopped before next turn"))`), stop after current turn (`Queued(Some("stop requested after current turn"))`), UI response with no pending request, UI response wrong request id, UI response too large, UI cancel decision, UI value decision, abort decision, and a between-turn sequence that rejects one command and still continues to process a following queued prompt before the next turn starts. These tests should not start fake pi.

21. Update `runner.gleam` active and between-turn command handling to call `operator_control`, then interpret returned effects in the old locations. Preserve existing emitted updates, pi sends, cleanup behavior, and reply timing. Keep side effects such as `client.send_extension_ui_cancel` in runner for this milestone if moving them immediately would make the slice too large.

22. Run format and tests. Record the milestone with `jj describe -m "Extract operator control decisions"`.

23. Create `src/scherzo/agent/turn_loop.gleam`. Move `ActiveCommandState`, `ActiveTurn`, `active_turn_loop`, `handle_active_command`, `handle_turn_record`, `handle_extension_ui_record`, `handle_blocking_ui_policy`, `handle_ui_response_command`, `handle_operator_ui_timeout`, `is_blocking_ui_method`, and `try_active`. Use `src/scherzo/agent/update.gleam` for record emission, lifecycle update construction, request update construction, and operator prompt redaction instead of defining those helpers in the turn loop. Move the pi-abort send/update part of `handle_abort_command`, but do not move workspace cleanup, `workspace.after_run`, tracker refresh, final classification, or generic `WorkerFailure` construction. The module should import `gleam/erlang/process`, `scherzo/pi/client`, `scherzo/pi/protocol`, `scherzo/agent/pi_event`, `scherzo/agent/operator_control`, `scherzo/agent/types`, `scherzo/agent/update`, `scherzo/agent/worker_command`, `scherzo/control/command`, and config/domain modules.

24. Define an explicit `turn_loop.Context` record rather than passing the entire runner context as many arguments. Use this shape as the starting point:

    pub type Context {
      Context(
        session: client.Session,
        issue_id: String,
        turn: Int,
        totals: domain.TokenTotals,
        read_timeout_ms: Int,
        stall_timeout_ms: Int,
        turn_deadline_ms: Int,
        stall_deadline_ms: Int,
        ui_request_policy: domain.UiRequestPolicy,
        ui_request_timeout_ms: Int,
        emit_update: fn(String, types.PiUpdate) -> Nil,
        command_subject: process.Subject(worker_command.Command),
        operator_state: operator_control.State,
        records: List(protocol.RpcRecord),
        secrets: List(String),
      )
    }

    Do not include `workspace_path` in this context. `run_attempt.gleam` already has the workspace path and must remain responsible for attaching it to `types.WorkerFailure`, running `workspace.after_run`, and emitting dropped prompts during cleanup. Do not import `scherzo/workspace` in `turn_loop.gleam`.

25. Define an explicit `turn_loop.TurnResult` that lets `run_attempt.gleam` remain the cleanup owner. A concrete shape is:

    pub type TurnResult {
      TurnCompleted(
        session: client.Session,
        records: List(protocol.RpcRecord),
        operator_state: operator_control.State,
        tokens: domain.TokenTotals,
      )
      TurnFailed(
        session: client.Session,
        reason: error.AgentRunnerError,
        records: List(protocol.RpcRecord),
        operator_state: operator_control.State,
        tokens: domain.TokenTotals,
      )
    }

    Use this shape as the starting contract. If implementation reveals an additional field is required to preserve current cleanup or update behavior, add only that field, record the reason in the Decision Log, and keep `session`, `records`, `operator_state`, `tokens`, and failure `reason` present. Do not return a prebuilt `types.WorkerFailure` from `turn_loop.gleam`, because that would move cleanup ownership and high-level failure construction out of `run_attempt.gleam`.

26. Update `runner.gleam` or the emerging `run_attempt.gleam` so between-turn code calls `turn_loop.run_active_turn(context)`. Interpret `TurnCompleted` by continuing the existing between-turn refresh/classification flow. Interpret `TurnFailed` by running the same cleanup/failure path currently used in `runner.gleam`.

27. Add or update tests. Existing `test/agent_worker_control_test.gleam` should remain the integration proof. Add `test/agent_turn_loop_test.gleam` if the module can be exercised without excessive fake setup; at minimum, test one direct turn-loop command ordering case or document in the plan that fake-pi integration tests cover the turn loop until further seams exist.

28. Run:

    ! grep -n "fn active_turn_loop\|fn handle_blocking_ui_policy\|fn handle_ui_response_command" src/scherzo/agent/runner.gleam
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    The grep check should produce no matches and exit zero because of the leading `!`. Record the milestone with `jj describe -m "Extract agent turn loop"`.

29. Create `src/scherzo/agent/run_attempt.gleam`. Move high-level runner functions and helpers from `runner.gleam`: `run_attempt`, `run_attempt_with_commands`, `run_attempt_with_command_ready`, `run_prompt_in_workspace`, `run_prepared`, `run_pi_loop`, `loop_turns`, `finish_after_turn`, `decide_after_refresh`, `finish_success`, cleanup/failure helpers, final classification, token addition, workspace prep calls, template rendering, compatibility probe integration, and conversion from turn-loop failures to `types.WorkerFailure`.

30. Keep `runner.gleam` as a small function facade that imports `agent/run_attempt.gleam` and forwards the public functions. Because constructor and variant references were migrated in Milestone 3, the facade does not need to preserve type constructors. It should not contain pi turn-loop logic, JSON decoding, port IO, workspace prep implementation, cleanup implementation, or UI policy implementation.

31. Update production callers: `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/workflow_run.gleam`, and any other function callers should import `scherzo/agent/run_attempt` for run functions; `src/scherzo/step_artifact.gleam`, `src/scherzo/orchestrator/event_publisher.gleam`, `src/scherzo/orchestrator/effect_runner.gleam`, `src/scherzo/handoff_format.gleam`, `src/scherzo/handoff.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/workflow_run.gleam`, and tests should import `scherzo/agent/types` for result/update types.

32. Run:

    ! grep -n "fn run_prepared\|fn run_pi_loop\|fn loop_turns\|fn finish_after_turn\|fn handle_between_turn_commands" src/scherzo/agent/runner.gleam
    wc -l src/scherzo/agent/runner.gleam src/scherzo/agent/pi_rpc.gleam src/scherzo/pi/protocol.gleam src/scherzo/pi/client.gleam src/scherzo/agent/update.gleam src/scherzo/agent/turn_loop.gleam src/scherzo/agent/run_attempt.gleam
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    `runner.gleam` should be a small facade. Record the milestone with `jj describe -m "Extract agent run attempt composition"`.

33. Migrate all repository-internal imports away from `scherzo/agent/pi_rpc` and `scherzo/agent/runner`. `src/scherzo/agent/probe.gleam` should import `scherzo/pi/client`; `src/scherzo/result_artifact.gleam` should import `scherzo/pi/protocol` if it only needs record decoding or types; tests should import `scherzo/pi/protocol`, `scherzo/pi/client`, `scherzo/agent/types`, and `scherzo/agent/run_attempt` directly. The only remaining use of `agent/pi_rpc` or `agent/runner` should be inside those facade files themselves if the files are intentionally kept.

34. Decide whether to delete `src/scherzo/agent/pi_rpc.gleam` and `src/scherzo/agent/runner.gleam` or keep them as compatibility facades. If kept, each facade should be short, documented, and contain no implementation logic. If deleted, update all imports and record the decision in the Decision Log.

35. Run final structural checks from the repository root. These checks must fail if a forbidden match remains; do not append `|| true` to checks that are meant to enforce acceptance.

    test -f src/scherzo/pi/protocol.gleam
    test -f src/scherzo/pi/client.gleam
    test -f src/scherzo/agent/types.gleam
    test -f src/scherzo/agent/update.gleam
    test -f src/scherzo/agent/operator_control.gleam
    test -f src/scherzo/agent/turn_loop.gleam
    test -f src/scherzo/agent/run_attempt.gleam
    sh -c 'if grep -R "^import scherzo/agent/pi_rpc" -n src --include="*.gleam"; then exit 1; fi'
    sh -c 'if grep -R "^import scherzo/agent/pi_rpc" -n test --include="*.gleam"; then exit 1; fi'
    sh -c 'if grep -R "^import scherzo/agent/runner" -n src test --include="*.gleam"; then exit 1; fi'
    sh -c 'if test -f src/scherzo/pi/protocol.gleam; then if grep -n "import scherzo/port\|import gleam/erlang/process\|import scherzo/agent/\|import scherzo/tracker\|import scherzo/template\|import scherzo/workspace" src/scherzo/pi/protocol.gleam; then exit 1; fi; fi'
    sh -c 'if test -f src/scherzo/agent/pi_rpc.gleam; then if grep -n "import gleam/json\|import gleam/dynamic/decode\|import scherzo/port\|fn read_turn_line\|fn read_until_response\|fn read_events_until_agent_end\|fn record_decoder\|type MessageObject" src/scherzo/agent/pi_rpc.gleam; then exit 1; fi; fi'
    sh -c 'if test -f src/scherzo/agent/runner.gleam; then if grep -n "fn active_turn_loop\|fn handle_blocking_ui_policy\|fn handle_ui_response_command\|fn run_prepared\|fn run_pi_loop\|fn update_from_record\|fn normalize_tool_text" src/scherzo/agent/runner.gleam; then exit 1; fi; fi'
    sh -c 'if test -f src/scherzo/pi/client.gleam; then if grep -n "prompt_with_ui_policy\|UiRequestPolicy\|import scherzo/agent/runner\|import scherzo/workspace\|import scherzo/tracker" src/scherzo/pi/client.gleam; then exit 1; fi; fi'
    sh -c 'if test -f src/scherzo/agent/update.gleam; then if grep -n "import scherzo/pi/client\|import scherzo/workspace\|import scherzo/tracker\|import scherzo/agent/runner\|import scherzo/agent/run_attempt\|import scherzo/agent/turn_loop" src/scherzo/agent/update.gleam; then exit 1; fi; fi'
    sh -c 'if test -f src/scherzo/agent/pi_rpc_legacy.gleam; then echo "remove temporary pi_rpc_legacy.gleam before final acceptance"; exit 1; fi'
    wc -l src/scherzo/agent/runner.gleam src/scherzo/agent/pi_rpc.gleam 2>/dev/null || true

    Production and test code should not import `agent/pi_rpc`; repository-internal code and tests should not import `agent/runner` after the function/type migration. Facades, if present, should not contain direct JSON decoder, port IO, update-construction helpers, or turn-loop implementation details. The line counts for facade files should be dramatically lower than the original 2,046 and 1,270 lines.

36. Run final validation:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

    Both commands must exit zero, and the test command must report `no failures`. Update this plan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective. Record the final change with a message such as `Split pi protocol, client, and agent turn loop`.

## Testing and Falsifiability

The protocol extraction is falsified if decoding the same JSON line produces different `RpcRecord` fields. Tests must cover response ids, session ids, message deltas, extension UI request messages, assistant messages from `agent_end`, captured tool calls/results from `test/fixtures/pi_tool_events_captured.jsonl`, top-level and nested tool execution aliases, token totals, and raw JSON preservation. It is also falsified if `src/scherzo/pi/protocol.gleam` imports `scherzo/agent/pi_event`; raw wire strings belong in protocol records, and typed pi events belong in the agent update layer.

The client extraction is falsified if fake-pi process behavior changes. Tests must cover launch, set session name, auto retry, get state, prompt send, interleaved response records, abort, extension UI cancel/value, get session stats, malformed JSON, read timeout, turn timeout, stall timeout, short read timeouts until events arrive, and diagnostics on process exit. It is also falsified if `src/scherzo/pi/client.gleam` exposes `prompt_with_ui_policy`, imports `domain.UiRequestPolicy`, or contains JSON decoder implementation details beyond calling protocol functions.

The shared-types extraction is falsified if any production or test code still relies on `runner.WorkerSuccess(...)`, `runner.WorkerFailure(...)`, `runner.PiUpdate(...)`, or `runner.FinalTerminal` after the type definitions have moved. Run the constructor searches in the concrete steps and require them to be empty or intentionally limited to a temporary compatibility test that is deleted before final acceptance.

The update-helper extraction is falsified if event subscribers see different `types.PiUpdate` values for the same protocol records. Tests must cover lifecycle updates, token updates, pi session started updates, redacted operator prompt messages, raw JSON redaction, request id/method propagation for blocking UI requests, and tool input/output/status redaction and truncation. It is also falsified if `src/scherzo/agent/update.gleam` imports `run_attempt`, `turn_loop`, `runner`, `workspace`, `tracker`, or `pi/client`; it should convert records and construct updates, not orchestrate behavior.

The operator-control extraction is falsified if worker command behavior changes. Tests must cover abort during active turn, prompt queueing during active turn and sending next turn, prompt queue full, oversized prompt rejection, stop-before-next-turn, stop-after-current-turn, UI response with pending request, UI response with no pending request, UI response wrong request id, UI response too large, UI timeout, and dropped prompts on abort/cleanup. Unit tests must assert the exact `worker_command.Reply` variant and message for active and between-turn modes.

The turn-loop extraction is falsified if operator commands are processed in a different order relative to pi reads, if blocking UI policy emits different updates or sends different pi commands, if skipped interleaved records are no longer emitted, or if cleanup happens inside `turn_loop.gleam` instead of the run-attempt layer. Existing fake-pi integration tests plus any direct `test/agent_turn_loop_test.gleam` coverage must catch these differences.

The run-attempt extraction is falsified if existing `test/agent_runner_test.gleam`, `test/agent_worker_control_test.gleam`, daemon tests, workflow run tests, handoff tests, event publisher tests, step artifact tests, and result artifact tests fail or need weaker assertions. Do not weaken tests to make extraction pass.

The structural claim is falsified if `pi/protocol.gleam` imports `scherzo/port` or `scherzo/agent`, if `pi/client.gleam` imports agent, workspace, tracker, template, or UI-policy modules, if `agent/update.gleam` imports client or orchestration modules, if `runner.gleam` still contains `active_turn_loop`, update-construction helpers, or high-level run composition, or if `pi_rpc.gleam` still contains read loops and decoder internals.

## Validation and Acceptance

Acceptance requires these commands from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Both must exit zero. The test command must report `no failures`; the 2026-05-03 baseline was `556 passed, no failures`. Expected fake-pi tests should still exercise real subprocess IO.

Structural acceptance requires the final structural checks from Concrete Step 35. The checks must be written so they fail on forbidden matches and only tolerate missing facade files. In particular, do not use `grep ... || true` for a check that is supposed to prove a forbidden symbol is absent.

If `agent/pi_rpc.gleam` or `agent/runner.gleam` remain, they must be compatibility facades only. If the project owner prefers deletion, delete them and make the relevant checks pass because the files do not exist.

Behavior acceptance requires no change to pi JSON command strings, decoded fake-pi events, token totals, tool extraction, raw JSON redaction, workspace hook execution, operator command replies, UI policy behavior, final classification, or cleanup behavior.

## Rollout, Recovery, and Idempotence

This is an internal refactor with no data migration. Roll out as a normal code change after tests pass. Each milestone should be green and separately described so it can be reverted independently.

If the client extraction causes timeout regressions, revert the `pi/client.gleam` milestone and keep the protocol extraction if it is green. If legacy prompt/UI-policy tests are the only blocker, keep the low-level client pure and move or rewrite those tests rather than adding policy back to `pi/client.gleam`. If the turn-loop extraction causes operator-control regressions, revert only that milestone and keep protocol/client/type extractions. Do not leave duplicated active turn implementations in production code across a commit.

The steps are safe to repeat. Running format, tests, and grep checks multiple times is safe. Test temporary files should stay under `test/tmp/...`, matching existing convention.

## Artifacts and Notes

Current file sizes at the 2026-05-03 plan review:

    2046 src/scherzo/agent/runner.gleam
    1270 src/scherzo/agent/pi_rpc.gleam

Current baseline test output at the same review:

    556 passed, no failures

Current `pi_rpc.gleam` public surface includes `Session`, `RpcRecord`, command encoders, `decode_record`, `launch`, `prompt`, `prompt_with_ui_policy`, `send_prompt`, `read_turn_record`, `send_abort`, UI response helpers, `get_session_stats`, and `terminate`.

Current `runner.gleam` public surface includes `FinalClassification`, `WorkerSuccess`, `WorkerFailure`, `PiUpdate`, `run_attempt`, `run_attempt_with_commands`, `run_attempt_with_command_ready`, and `run_prompt_in_workspace`.

Keep the fake-pi fixture `test/fixtures/fake_pi_rpc.sh` as the integration proof. Do not replace all fake-pi tests with pure unit tests; the port/process boundary is exactly the risky behavior this plan must continue to exercise.

## Interfaces and Dependencies

In `src/scherzo/pi/protocol.gleam`, define or move:

    pub type RpcRecord {
      RpcRecord(
        type_: String,
        id: Option(String),
        command: Option(String),
        success: Option(Bool),
        session_id: Option(String),
        delta: Option(String),
        message: Option(String),
        method: Option(String),
        tokens: domain.TokenTotals,
        tool_name: Option(String),
        tool_input: Option(String),
        tool_output: Option(String),
        tool_status: Option(String),
        assistant_messages: List(String),
        raw_json: String,
      )
    }

    pub type Data {
      Data(
        session_id: Option(String),
        tokens: domain.TokenTotals,
        tool_name: Option(String),
        tool_input: Option(String),
        tool_output: Option(String),
        tool_status: Option(String),
      )
    }

    pub fn encode_set_session_name(id: String, name: String) -> String
    pub fn encode_set_auto_retry(id: String, enabled: Bool) -> String
    pub fn encode_get_state(id: String) -> String
    pub fn encode_prompt(id: String, message: String) -> String
    pub fn encode_abort(id: String) -> String
    pub fn encode_get_session_stats(id: String) -> String
    pub fn encode_extension_ui_response(id: String) -> String
    pub fn encode_extension_ui_value_response(id: String, value: String) -> String
    pub fn decode_record(line: String) -> Result(RpcRecord, error.PiRpcError)

This module may import `gleam/json`, `gleam/dynamic/decode`, `gleam/list`, `gleam/option`, `gleam/string`, `scherzo/domain`, and `scherzo/error`. It must not import `scherzo/port`, `gleam/erlang/process`, `scherzo/agent/pi_event`, `scherzo/agent/worker_command`, `scherzo/tracker`, `scherzo/template`, or workspace modules.

In `src/scherzo/pi/client.gleam`, define or move:

    pub type Session {
      Session(
        process: port.Process,
        command: String,
        cwd: String,
        session_id: Option(String),
        next_id: Int,
      )
    }

    pub fn launch(command: String, cwd: String, session_name: String, auto_retry: Bool, read_timeout_ms: Int) -> Result(Session, error.PiRpcError)
    pub fn send_prompt(session: Session, message: String, read_timeout_ms: Int) -> Result(#(Session, List(protocol.RpcRecord)), error.PiRpcError)
    pub fn read_turn_record(session: Session, read_timeout_ms: Int, turn_deadline_ms: Int, stall_deadline_ms: Int) -> Result(#(Session, Option(protocol.RpcRecord)), error.PiRpcError)
    pub fn send_abort(session: Session, read_timeout_ms: Int) -> Result(#(Session, List(protocol.RpcRecord)), error.PiRpcError)
    pub fn send_extension_ui_cancel(session: Session, request_id: String, read_timeout_ms: Int) -> Result(#(Session, List(protocol.RpcRecord)), error.PiRpcError)
    pub fn send_extension_ui_value(session: Session, request_id: String, value: String, read_timeout_ms: Int) -> Result(#(Session, List(protocol.RpcRecord)), error.PiRpcError)
    pub fn get_session_stats(session: Session, read_timeout_ms: Int) -> Result(#(Session, domain.TokenTotals), error.PiRpcError)
    pub fn terminate(session: Session) -> Result(Nil, error.PiRpcError)

This module may import `scherzo/pi/protocol`, `scherzo/port`, `scherzo/error`, `scherzo/domain`, and stdlib modules. It must not import `scherzo/agent/runner`, `scherzo/agent/turn_loop`, `scherzo/agent/pi_event`, `scherzo/template`, `scherzo/workspace`, tracker modules, or `domain.UiRequestPolicy`-driven legacy prompt helpers. It must not expose `prompt` or `prompt_with_ui_policy`.

In `src/scherzo/agent/types.gleam`, define:

    pub type FinalClassification {
      FinalActive
      FinalTerminal
      FinalNonActive
    }

    pub type WorkerSuccess {
      WorkerSuccess(
        final_issue: Option(domain.Issue),
        final_classification: FinalClassification,
        workspace_path: String,
        tokens: domain.TokenTotals,
        turns: Int,
        result: domain.ResultArtifact,
      )
    }

    pub type WorkerFailure {
      WorkerFailure(
        reason: error.AgentRunnerError,
        workspace_path: Option(String),
        tokens: domain.TokenTotals,
        final_issue: Option(domain.Issue),
      )
    }

    pub type PiUpdate {
      PiUpdate(
        event: pi_event.PiEvent,
        message: Option(String),
        raw_json: Option(session_event.RedactedRawJson),
        turn: Option(Int),
        request_id: Option(String),
        method: Option(String),
        pi_session_id: Option(String),
        tokens: domain.TokenTotals,
        tool_name: Option(String),
        tool_input: Option(String),
        tool_output: Option(String),
        tool_status: Option(String),
      )
    }

`PiUpdate.event` must remain `pi_event.PiEvent`. After the move, production and test constructors should use `types.WorkerSuccess(...)`, `types.WorkerFailure(...)`, `types.PiUpdate(...)`, and `types.FinalTerminal` rather than `runner`-qualified constructors or variants.

In `src/scherzo/agent/update.gleam`, define the shared update helpers with these public names:

    pub fn lifecycle(name: pi_event.PiEvent) -> types.PiUpdate
    pub fn lifecycle_with_message(name: pi_event.PiEvent, message: Option(String)) -> types.PiUpdate
    pub fn lifecycle_with_request(name: pi_event.PiEvent, message: Option(String), request_id: String, method: String, turn: Int) -> types.PiUpdate
    pub fn pi_session_started(pi_session_id: Option(String)) -> types.PiUpdate
    pub fn token(name: pi_event.PiEvent, turn: Int, tokens: domain.TokenTotals) -> types.PiUpdate
    pub fn from_record(record: protocol.RpcRecord, turn: Int, secrets: List(String)) -> types.PiUpdate
    pub fn emit_records(issue_id: String, records: List(protocol.RpcRecord), turn: Int, secrets: List(String), emit_update: fn(String, types.PiUpdate) -> Nil) -> Nil
    pub fn redact_operator_message(message: String, secrets: List(String)) -> String

This module may import `gleam/list`, `gleam/option`, `gleam/string`, `scherzo/agent/pi_event`, `scherzo/agent/types`, `scherzo/domain`, `scherzo/log`, `scherzo/pi/protocol`, and `scherzo/session/redaction`. It must not import `scherzo/pi/client`, `scherzo/workspace`, `scherzo/tracker`, `scherzo/template`, `scherzo/agent/runner`, `scherzo/agent/run_attempt`, or `scherzo/agent/turn_loop`.

In `src/scherzo/agent/operator_control.gleam`, define pending UI and prompt queue state plus command-decision helpers. This module may import `gleam/erlang/process`, `scherzo/agent/worker_command`, and `scherzo/control/command`; it should not import pi client, tracker, template, workspace, handoff, run-attempt, or runner modules. Its public API should return explicit effects for replies, queued-prompt update emission, abort, stop, and UI response sends.

In `src/scherzo/agent/turn_loop.gleam`, expose:

    pub fn run_active_turn(context: Context) -> TurnResult

The `Context` type must be an explicit record containing the fields listed in Concrete Step 24 and must not require importing `runner.gleam`. `TurnResult` must distinguish completed turns from turn-local failures and carry enough session, record, operator state, and token data for `run_attempt.gleam` to perform cleanup and construct `types.WorkerFailure`.

In `src/scherzo/agent/run_attempt.gleam`, expose:

    pub fn run_attempt(issue: domain.Issue, attempt: Option(Int), prompt_template: String, config: domain.EffectiveConfig, tracker_client: tracker.Client, emit_update: fn(String, types.PiUpdate) -> Nil) -> Result(types.WorkerSuccess, types.WorkerFailure)
    pub fn run_attempt_with_commands(issue: domain.Issue, attempt: Option(Int), prompt_template: String, config: domain.EffectiveConfig, tracker_client: tracker.Client, emit_update: fn(String, types.PiUpdate) -> Nil, command_subject: process.Subject(worker_command.Command)) -> Result(types.WorkerSuccess, types.WorkerFailure)
    pub fn run_attempt_with_command_ready(issue: domain.Issue, attempt: Option(Int), prompt_template: String, config: domain.EffectiveConfig, tracker_client: tracker.Client, emit_update: fn(String, types.PiUpdate) -> Nil, command_subject: process.Subject(worker_command.Command), on_command_ready: fn() -> Nil) -> Result(types.WorkerSuccess, types.WorkerFailure)
    pub fn run_prompt_in_workspace(issue: domain.Issue, prompt: String, config: domain.EffectiveConfig, tracker_client: tracker.Client, emit_update: fn(String, types.PiUpdate) -> Nil, command_subject: process.Subject(worker_command.Command), on_command_ready: fn() -> Nil, workspace_path: String) -> Result(types.WorkerSuccess, types.WorkerFailure)

The old `src/scherzo/agent/runner.gleam` facade, if kept, should forward to these functions. Do not rely on it to re-expose type constructors; constructor and variant call sites should already import `agent/types.gleam` directly.

## Plan Revision Notes

2026-05-02 review update: refreshed baseline facts, removed the prior-plan dependency around typed pi events, tightened module boundaries for `pi/client.gleam` and `turn_loop.gleam`, made the `agent/types.gleam` constructor migration explicit, and replaced non-failing structural greps with checks that fail on forbidden matches. These changes make the plan more self-contained, keep the low-level pi client free of UI policy, and reduce the risk of breaking callers that construct `runner`-qualified result/update types.

2026-05-03 review update: re-verified the current baseline (`556 passed, no failures`) and current file sizes (2,046-line `runner.gleam`, 1,270-line `pi_rpc.gleam`); added `src/scherzo/agent/update.gleam` as the explicit owner of `PiUpdate` construction, raw JSON redaction, operator prompt redaction, and tool text truncation; expanded the interfaces to concrete type fields and public function signatures; included the current pi RPC characterization tests; and strengthened structural checks so tests also migrate away from `agent/pi_rpc`/`agent/runner` and so protocol/update modules cannot import higher-level orchestration modules.
