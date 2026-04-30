# Replace stringly internal state machine values with typed variants

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo's internal state machines should use Gleam custom types for states, statuses, event names, retry reasons, park reasons, and worker exit reasons. Strings should still exist at external boundaries such as Linear API payloads, pi RPC JSON, control API JSON, logs, templates, and terminal output, but those strings should be parsed into internal types once and converted back to strings only when leaving the typed core.

The visible proof is that typos like `"sucess"`, `"retry poll failed"`, or `"worker_dwon"` cannot silently change scheduler behavior. The step scheduler should branch on `StepSucceeded` or `StepFailed`, not on `artifact.status == "success"`. The daemon should publish `WorkerExited(Normal)` or `WorkerExited(Failed)`, not hand around raw strings. `runner.PiUpdate` should carry a `PiEvent` variant, and daemon event classification should be exhaustive over known pi events. Tracker kind should be `LinearTracker`, not the string `"linear"`. The retry entry field currently named `due_at_ms` should stop lying: it stores a delay, so it should either be renamed to `delay_ms` or populated with an actual absolute due time. This plan chooses the smaller safe change: rename it to `delay_ms`.

## Problem Framing and Constraints

Scherzo is mostly a state machine. It polls tracker issues, schedules retries, starts workers, handles worker exits, runs workflow DAG steps, publishes EventHub events, and accepts operator commands. In the current tree many of those internal transitions are keyed by strings. The current `src/scherzo/domain.gleam` stores `Issue.state` as `String`, `TrackerConfig.kind` as `String`, `ParkedEntry.reason` as `String`, and `RetryEntry.due_at_ms` even though `src/scherzo/orchestrator/core.gleam` fills it with a delay. `src/scherzo/session/event.gleam` stores `EventPayload.name` as `String` and `SessionStatus.Exited(reason: String)`. `src/scherzo/agent/runner.gleam` stores `PiUpdate.event` as `String`. `src/scherzo/step_artifact.gleam` stores `StepArtifact.status` as `"success"` or `"failure"`. `src/scherzo/workflow_scheduler.gleam`, `src/scherzo/workflow_run.gleam`, and `src/scherzo/orchestrator/daemon.gleam` branch on that status string. `src/scherzo/orchestrator/core.gleam` hard-codes the blocker policy with `normalize(issue.state) == "todo"` and schedules retries with raw reason strings such as `"failure"`, `"continuation"`, `"retry poll failed"`, and `"no available orchestrator slots"`.

This matters because string typos become behavior changes exactly where correctness matters most. A misspelled step status changes whether a DAG continues. A misspelled retry reason changes logs and events. A raw issue state string makes it hard to tell whether a function expects a user-visible Linear state, a normalized state key, or a config state. A raw pi event string makes daemon event classification non-exhaustive.

The plan must preserve external compatibility. JSON still says `"success"`, `"failure"`, `"normal"`, `"worker_down"`, and pi event names in the same places as today. Template locals still expose `steps.<id>.status` as `"success"` or `"failure"`. Logs still use stable strings. Control protocol decoders must still accept old event and status strings. The change is internal representation and exhaustiveness, not a protocol migration.

## Strategy Overview

Migrate from the smallest and safest stringly values to the broadest values. Start with `StepStatus`, because it is localized and directly controls workflow scheduling. Then introduce typed retry, park, and worker exit reasons because those strings drive daemon/session lifecycle behavior. Then type pi events and EventHub event names. Move tracker kind and issue state later because they touch config parsing, Linear decoding, workflow policy, blockers, and many tests.

Use explicit conversion modules. Every new type should have a small set of functions such as `to_string`, `from_string`, `is_success`, or `normalize`. String conversion belongs at boundaries: config parsing, Linear decoding, pi RPC decoding, EventHub/control JSON decoding and encoding, logging, and template locals. Internal branches should pattern match on variants.

This plan can be implemented before or after `docs/plans/domain-decomposition.md`. If the domain decomposition plan has already moved types out of `src/scherzo/domain.gleam`, use the new owner modules named there. If not, introduce the typed modules in the current tree and update `domain.gleam` fields directly. Do not use this plan as an excuse to split all of `domain.gleam`; that is a separate plan.

## Alternatives Considered

One alternative is to add constants for strings, such as `const step_success = "success"`. That reduces typo risk in one file but still gives no exhaustiveness checking and still lets any arbitrary string represent a scheduler state.

Another alternative is to type every string in the repository at once, including command protocol reasons, Linear state names, tool statuses, pi RPC record types, log event names, and errors. That is too broad. Many strings are true integration-boundary strings, and changing all of them in one pass would make regressions hard to isolate.

A third alternative is to enumerate every possible Linear issue state as variants. That is not correct because Linear states are configured by the user's workspace. The right internal type for issue state is a parsed wrapper around the external state name with normalization helpers, not a closed enum of all possible state names.

A fourth alternative is to fix `RetryEntry.due_at_ms` by computing absolute due timestamps everywhere. That might be useful later, but the current field is not read anywhere outside construction. Renaming it to `delay_ms` is the smallest truthful fix. If future scheduling needs absolute due time, add a separate `due_at_ms` field then and populate it from a clock.

## Risks and Countermeasures

The main behavior risk is changing serialized strings. Countermeasure: each typed value must have a `to_string` function that returns the exact current strings, and existing JSON/protocol/template tests must continue to pass. Add direct conversion tests for every new type.

The main compile-risk is large constructor churn. Countermeasure: migrate one type family at a time and run format/tests after each milestone. Do not combine `StepStatus`, `PiEvent`, and `IssueState` in one commit.

The main design risk is over-typing values that are truly open-ended. Countermeasure: use closed ADTs only for closed internal vocabularies such as step status, tracker kind, retry reason, park reason, and known worker exit reasons. For open external vocabularies, use wrappers with `Unknown(String)` or `Raw(String)` variants. For Linear issue states, use a typed wrapper that preserves the original state string and normalized key.

The main import-cycle risk appears if session/event types import orchestrator reason types. Countermeasure: put session exit reason types in a session-owned module such as `src/scherzo/session/reason.gleam` or directly in `src/scherzo/session/event.gleam`. Orchestrator code may import session reason types; session modules must not import orchestrator modules.

The main UX risk is losing user-supplied park reasons. Countermeasure: model park reasons as an ADT that includes `OperatorPark(String)` or `CustomParkReason(String)`. Only known internal reasons become closed variants; user-entered reason text remains preserved and encoded at logging/control boundaries.

## Progress

- [x] (2026-04-30 10:47Z) Verified the current test baseline earlier in this work session with `direnv exec . gleam test`; it passed with `377 passed, no failures`.
- [x] (2026-04-30 11:45Z) Fact-checked current stringly examples in `src/scherzo/domain.gleam`, `src/scherzo/session/event.gleam`, `src/scherzo/agent/runner.gleam`, `src/scherzo/step_artifact.gleam`, `src/scherzo/workflow_scheduler.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/orchestrator/daemon.gleam`, and `src/scherzo/orchestrator/core.gleam`.
- [x] (2026-04-30 11:55Z) Drafted this plan with milestones ordered from localized status strings to broader tracker issue state typing.
- [ ] Milestone 0: add characterization tests for current external string encodings.
- [ ] Milestone 1: introduce `StepStatus` and remove internal step status string comparisons.
- [ ] Milestone 2: introduce typed retry reasons, park reasons, worker stop reasons, and rename retry delay storage.
- [ ] Milestone 3: introduce typed session/worker exit reasons in EventHub session status.
- [ ] Milestone 4: introduce `PiEvent` for `runner.PiUpdate.event` and update daemon event classification.
- [ ] Milestone 5: introduce typed EventHub event names for lifecycle and pi events.
- [ ] Milestone 6: introduce typed tracker kind and issue state wrappers.
- [ ] Milestone 7: remove remaining internal string comparisons and write the retrospective.

## Surprises & Discoveries

- Observation: `RetryEntry.due_at_ms` is not read anywhere in current source or tests except at its definition and construction.
  Evidence: a search for `due_at_ms` found only `src/scherzo/domain.gleam` and `src/scherzo/orchestrator/core.gleam`. This makes renaming it to `delay_ms` low risk.

- Observation: Command status is already partially typed.
  Evidence: `src/scherzo/control/command.gleam` defines `CommandStatus` with variants `Applied`, `Queued`, `Rejected(reason)`, `NotFound`, and `NotAllowed(reason)`, plus boundary functions `status_to_string` and `status_from_string`. This plan should copy that pattern rather than replacing it.

- Observation: Some string values are intentionally open external vocabularies.
  Evidence: `src/scherzo/agent/pi_rpc.gleam` decodes `RpcRecord.type_` from pi RPC JSON, and `tool_status` may come from pi/tool payloads. This plan types known pi events with `UnknownPiEvent(String)` rather than rejecting unknown events.

## Decision Log

- Decision: Start with step artifact status.
  Rationale: It is a small, high-value change. The current scheduler branches on `artifact.status == "success"`, so this milestone proves the pattern before broader migrations.
  Date: 2026-04-30

- Decision: Keep external strings stable and put conversions at boundaries.
  Rationale: Scherzo already has control protocol, EventHub JSON, template locals, terminal rendering, and logs that operators may rely on. Internal typing should not become an external breaking change.
  Date: 2026-04-30

- Decision: Use a typed wrapper for Linear issue state rather than enumerating states.
  Rationale: Linear state names are workspace-specific and configurable. A closed enum would be wrong; the internal type should distinguish an issue state from arbitrary text while preserving the raw configured value.
  Date: 2026-04-30

- Decision: Rename `RetryEntry.due_at_ms` to `delay_ms` rather than computing an absolute due time in this plan.
  Rationale: The field currently stores delay and is not read. Renaming fixes the misleading invariant without changing scheduler behavior.
  Date: 2026-04-30

## Outcomes & Retrospective

(To be filled at major milestones and at completion.)

## Context and Orientation

The current central type module is `src/scherzo/domain.gleam`. It defines `Issue` with `state: String`, `TrackerConfig` with `kind: String`, `ParkedEntry` with `reason: String`, and `RetryEntry(issue_id, due_at_ms, timer_generation)`. The field `due_at_ms` is set from a delay in `src/scherzo/orchestrator/core.gleam` inside `schedule_retry`.

Step artifacts are built in `src/scherzo/step_artifact.gleam`. `StepArtifact.status` is currently a `String`. `from_agent_success` sets it to `"success"`. `from_command_result_with_truncation` sets it to `"success"` or `"failure"`. `artifact_locals` exposes that value to workflow templates as `steps.<step_id>.status`.

Workflow scheduling is implemented in `src/scherzo/workflow_scheduler.gleam` and `src/scherzo/workflow_run.gleam`. Both currently check `artifact.status == "success"` to decide whether a step succeeded, failed-but-continued, or failed fatally.

Pi update events are created in `src/scherzo/agent/runner.gleam`. `PiUpdate.event` is currently a `String`. Known names include `"probe_started"`, `"probe_finished"`, `"pi_session_started"`, `"turn_finished"`, `"message_start"`, `"message_update"`, `"message_end"`, `"tool_execution_start"`, `"tool_execution_update"`, `"tool_execution_end"`, `"message"`, `"extension_ui_request"`, `"extension_ui_response"`, `"agent_start"`, `"turn_start"`, `"turn_end"`, and `"agent_end"`. Unknown pi RPC record types are preserved as raw events.

EventHub session events are defined in `src/scherzo/session/event.gleam`. `EventPayload.kind` is already typed as `EventKind`, but `EventPayload.name` is a `String`. Session exit status is `Exited(reason: String)`. JSON conversion lives in `src/scherzo/session/json.gleam`, control protocol decoding/encoding lives in `src/scherzo/control/protocol.gleam`, and terminal rendering uses `src/scherzo/terminal/render.gleam`.

Daemon lifecycle publishing happens mostly in `src/scherzo/orchestrator/daemon.gleam`. It passes lifecycle names such as `"dispatch_started"`, `"worker_started"`, `"stop_requested"`, `"worker_exited"`, `"worker_down"`, `"retry_scheduled"`, `"operator_command"`, and `"step_started"`, and exit reasons such as `"normal"`, `"failed"`, `"operator_abort"`, `"worker_down"`, and `"stopped"`.

## Preconditions and Verified Facts

Before implementing, run from the repository root:

    jj status
    direnv exec . gleam test

At plan-authoring time, the test suite ended with:

    377 passed, no failures

Re-run these searches before coding because the tree may have changed:

    grep -R "artifact.status == \"success\"" -n src test --include='*.gleam'
    grep -R "event: String" -n src/scherzo/agent/runner.gleam
    grep -R "name: String" -n src/scherzo/session/event.gleam
    grep -R "kind: String" -n src/scherzo/domain.gleam src/scherzo/config/types.gleam 2>/dev/null || true
    grep -R "due_at_ms" -n src test --include='*.gleam'

At plan-authoring time, the status comparisons appeared in `workflow_scheduler.gleam`, `workflow_run.gleam`, and `orchestrator/daemon.gleam`; `PiUpdate.event` was a string in `agent/runner.gleam`; `EventPayload.name` was a string in `session/event.gleam`; tracker kind was a string in `domain.gleam`; and `due_at_ms` appeared only in `domain.gleam` and `orchestrator/core.gleam`.

This repository uses Jujutsu. Do not use mutating `git` commands. Use `jj status` for status, `jj describe -m "message"` to describe the current change, and `jj new` to start a new change after a green milestone.

## Scope Boundaries

In scope: `StepStatus`, `RetryReason`, `ParkReason`, `WorkerExitReason` or `SessionExitReason`, `PiEvent`, lifecycle event names, tracker kind, issue state wrappers, the blocker policy's `"todo"` comparison, and the retry delay field rename.

Out of scope: changing external JSON field names or string values; changing Linear state configuration syntax beyond parsing configured states into wrappers; changing command protocol reason strings broadly; changing log event names broadly; changing pi RPC wire strings; changing workflow template variable names; changing the retry algorithm; changing the daemon actor structure.

It is acceptable for external decoders to create `Unknown(String)` or `Raw(String)` variants when reading old or future data. It is not acceptable for internal scheduler branches to compare raw strings once the typed value exists.

## Milestones

Milestone 0 adds characterization tests for current string encodings. At the end, tests prove the external strings that must not change: step artifact template status strings, session exit reason JSON strings, pi event JSON names, command result status strings, and tracker kind parsing.

Milestone 1 introduces `StepStatus`. At the end, `StepArtifact.status` is a custom type, and no source file decides step success by comparing status to `"success"`.

Milestone 2 introduces typed orchestrator reasons and fixes retry delay naming. At the end, `core.ScheduleRetry`, `core.ParkIssue`, and `domain.ParkedEntry.reason` or `orchestrator/state.ParkedEntry.reason` use typed reasons. Logs and EventHub messages convert reasons with `to_string`. `RetryEntry.due_at_ms` is renamed to `delay_ms`.

Milestone 3 introduces typed session exit reasons. At the end, `hub.finish_session` and `SessionStatus.Exited` accept a typed reason, while EventHub JSON still emits the same `exit_reason` strings.

Milestone 4 introduces typed pi events. At the end, `runner.PiUpdate.event` is a `PiEvent`, daemon event classification pattern matches on variants, and unknown pi RPC event types are preserved with `UnknownPiEvent(raw)`.

Milestone 5 introduces typed EventHub event names. At the end, `EventPayload.name` is an event-name ADT and JSON/protocol conversion is the only place that turns it into or from a string.

Milestone 6 introduces typed tracker kind and issue state wrappers. At the end, config parsing parses tracker kind to `LinearTracker`, issue state strings from Linear/config are represented by an `IssueState` wrapper, state normalization lives in one module, and the blocker `"todo"` policy is represented by a named typed value.

Milestone 7 removes leftover internal string comparisons and records the outcome.

## Plan of Work

Start by adding tests for boundary strings so the refactor can be aggressive internally without accidentally changing output. Then change step status because it is local. Next change reasons because they are mostly inside orchestrator and session lifecycle paths. Then change pi events and EventHub names because they affect session JSON and control protocol. Finally change tracker kind and issue state because they touch config parsing, Linear decoding, workflow policy, and orchestrator dispatch.

For each type, create the type and conversion helpers first. Update internal records to store the type. Update all compile errors by replacing string constructors with variants. Update boundary code to call `to_string` or `from_string`. Run grep checks that prove old comparisons are gone. Run format and tests. Commit only when green.

## Concrete Steps

1. From the repository root, run `jj status`. If unrelated source changes exist, stop and either move to a clean workspace or record them in this plan.

2. Run `direnv exec . gleam test`. Expect the suite to end with `no failures`. The count was `377 passed` at plan authoring.

3. Add or update tests before code changes. In `test/step_artifact_test.gleam`, assert that a successful command step still exposes template local `steps.<id>.status` as `"success"` and a failing command still exposes `"failure"`. In `test/session_event_test.gleam` or `test/control_protocol_test.gleam`, assert that a session exited normally still encodes `exit_reason` as `"normal"`. In `test/agent_runner_test.gleam` or an added `test/pi_event_test.gleam`, assert known pi event names convert to their current strings.

4. Run `direnv exec . gleam test` and confirm these characterization tests pass before changing implementation.

5. In `src/scherzo/step_artifact.gleam`, define:

    pub type StepStatus {
      StepSucceeded
      StepFailed
    }

    pub fn status_to_string(status: StepStatus) -> String
    pub fn status_from_exit(exit_code: Int, timed_out: Bool) -> StepStatus
    pub fn succeeded(status: StepStatus) -> Bool

    `status_to_string(StepSucceeded)` must return `"success"`; `status_to_string(StepFailed)` must return `"failure"`.

6. Change `StepArtifact.status` from `String` to `StepStatus`. In `from_agent_success`, set `status: StepSucceeded`. In `from_command_result_with_truncation`, compute a `StepStatus` with `status_from_exit`, then use `status_to_string(status)` when building `summary_text`.

7. In `artifact_locals`, change `template.VString(artifact.status)` to `template.VString(status_to_string(artifact.status))`.

8. Update `src/scherzo/workflow_scheduler.gleam`, `src/scherzo/workflow_run.gleam`, and `src/scherzo/orchestrator/daemon.gleam` to call `step_artifact.succeeded(artifact.status)` instead of comparing to `"success"`.

9. Run:

    grep -R "artifact.status == \"success\"" -n src test --include='*.gleam'
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    The grep should return no matches, and tests should pass. Record this milestone with `jj describe -m "Type workflow step artifact status"`, then run `jj new` if keeping separate changes.

10. Create `src/scherzo/orchestrator/reason.gleam`. Define typed reasons:

    pub type RetryReason {
      RetryAfterFailure
      RetryAfterContinuation
      RetryNoSlots
      RetryPollFailed
    }

    pub type ParkReason {
      ParkMaxRetryAttempts
      ParkMaxSessionsPerIssue
      ParkOperator(reason: String)
    }

    pub type StopReason {
      StopOperatorAbort
      StopAfterCurrentTurn
      StopNoAvailableSlot
      StopCustom(reason: String)
    }

    Add `retry_to_string`, `park_to_string`, and `stop_to_string` with the exact strings used today.

11. In `src/scherzo/orchestrator/core.gleam`, change `core.Effect.ScheduleRetry.reason` from `String` to `reason.RetryReason`, `core.Effect.ParkIssue.reason` from `String` to `reason.ParkReason`, and `core.Effect.StopWorker.reason` from `String` to `reason.StopReason` if the call sites are manageable. If `StopWorker` proves too broad, defer only `StopWorker` and record the reason in the Decision Log; do not defer retry and park reasons.

12. Update `schedule_retry` to accept `RetryReason`. Replace calls: worker failure uses `RetryAfterFailure`, continuation uses `RetryAfterContinuation`, retry poll failure uses `RetryPollFailed`, and no-slot scheduling uses `RetryNoSlots`.

13. Change `ParkedEntry.reason` in `src/scherzo/domain.gleam` or `src/scherzo/orchestrator/state.gleam` to `reason.ParkReason`. Replace internal park calls: max retries uses `ParkMaxRetryAttempts`, max sessions uses `ParkMaxSessionsPerIssue`, and operator park commands convert user text to `ParkOperator(reason)` at the command boundary.

14. Rename `RetryEntry.due_at_ms` to `delay_ms` in `src/scherzo/domain.gleam` or `src/scherzo/orchestrator/state.gleam`, and update `core.schedule_retry` to set `delay_ms: delay_ms`.

15. Update daemon and service logging to call `reason.retry_to_string`, `reason.park_to_string`, or `reason.stop_to_string` when producing log fields or EventHub message text. Keep log strings unchanged.

16. Update `test/domain_test.gleam` or `test/orchestrator_state_test.gleam` so parked issue assertions compare `reason.ParkMaxRetryAttempts` as a value and separately assert `reason.park_to_string(reason.ParkMaxRetryAttempts) == "max_retry_attempts"`. Add an orchestrator core test that the retry entry stores `delay_ms`, not `due_at_ms`.

17. Run:

    grep -R "due_at_ms" -n src test --include='*.gleam'
    grep -R "\"retry poll failed\"\|\"continuation\"\|\"max_retry_attempts\"" -n src/scherzo/orchestrator src/scherzo/domain.gleam src/scherzo/orchestrator/state.gleam 2>/dev/null || true
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    `due_at_ms` should be gone. Remaining reason strings should live only in conversion functions or tests that assert conversion. Record the milestone.

18. Create `src/scherzo/session/reason.gleam` or add the type directly to `src/scherzo/session/event.gleam`. Prefer a separate module if both daemon and protocol code need it. Define:

    pub type WorkerExitReason {
      Normal
      Failed
      OperatorAbort
      WorkerDown
      Stopped
      Unknown(reason: String)
    }

    pub fn to_string(reason: WorkerExitReason) -> String
    pub fn from_string(reason: String) -> WorkerExitReason

    `to_string` must preserve current strings: `"normal"`, `"failed"`, `"operator_abort"`, `"worker_down"`, and `"stopped"`.

19. Change `session/event.gleam` so `SessionStatus.Exited` carries `WorkerExitReason` instead of `String`. Change `exit_reason` to return `Option(WorkerExitReason)` or add `exit_reason_string` for JSON code. Keep `status_to_string(Exited(_)) == "exited"`.

20. Change `src/scherzo/session/hub.gleam` so `finish_session` accepts `WorkerExitReason`. Update all daemon, workflow step, and protocol call sites to pass variants. For decoded external protocol data, use `from_string` so old values become known variants or `Unknown(raw)`.

21. Update `src/scherzo/session/json.gleam`, `src/scherzo/control/protocol.gleam`, and terminal rendering to call `session_reason.to_string` when emitting strings. Add tests that `Unknown("x")` round-trips to `"x"` in JSON if decoded from old/future data.

22. Run format and tests. Record the milestone.

23. Create `src/scherzo/agent/pi_event.gleam`. Define variants for known pi events and an unknown case:

    pub type PiEvent {
      ProbeStarted
      ProbeFinished
      PiSessionStarted
      TurnFinished
      MessageStart
      MessageUpdate
      MessageEnd
      ToolExecutionStart
      ToolExecutionUpdate
      ToolExecutionEnd
      Message
      ExtensionUiRequest
      ExtensionUiResponse
      AgentStart
      TurnStart
      TurnEnd
      AgentEnd
      UnknownPiEvent(name: String)
    }

    Add `to_string`, `from_string`, `is_blocking_ui_request`, `is_token_stats`, and any small predicates needed by daemon classification.

24. Change `runner.PiUpdate.event` in `src/scherzo/agent/runner.gleam` from `String` to `pi_event.PiEvent`. Update `lifecycle_update`, `lifecycle_update_with_message`, `lifecycle_update_with_request`, `pi_session_started_update`, `token_update`, and `update_from_record`. Known internal calls should pass variants. Pi RPC record strings should be parsed once with `pi_event.from_string(record.type_)`.

25. Update daemon event classification functions `kind_for_update`, `status_for_update`, and `pi_type_for_update` to pattern match on `update.event`. Unknown events with raw JSON should still become `session_event.PiRaw`; known events should preserve current `EventKind` and status behavior.

26. Update any tests that assert `update.event == "..."` to assert variants and separately assert `pi_event.to_string(update.event) == "..."` where external string stability matters.

27. Run:

    grep -R "event: String" -n src/scherzo/agent/runner.gleam
    grep -R "update.event" -n src/scherzo --include='*.gleam'
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    The first grep should return no matches. Inspect the second grep and ensure remaining branches pattern match on variants or call `pi_event.to_string` only at boundaries. Record the milestone.

28. In `src/scherzo/session/event.gleam`, define typed event names. Use a shape like:

    pub type LifecycleEventName {
      DispatchStarted
      WorkerStarted
      StopRequested
      WorkerExited
      WorkerDown
      RetryScheduled
      OperatorCommand
      StepStarted
      CustomLifecycleEvent(name: String)
    }

    pub type EventName {
      LifecycleName(LifecycleEventName)
      PiName(pi_event.PiEvent)
      RawEventName(name: String)
    }

    Add `name_to_string` and `name_from_string`. The raw/custom variants preserve compatibility with events decoded from control protocol JSON or future pi versions.

29. Change `EventPayload.name` from `String` to `EventName`. Change `empty_payload` to accept `EventName`. Change `session/json.gleam` and `control/protocol.gleam` to convert through `name_to_string` and `name_from_string`.

30. Update daemon `publish_lifecycle` to accept `LifecycleEventName`. Replace direct string names such as `"dispatch_started"`, `"worker_started"`, `"worker_exited"`, `"worker_down"`, `"retry_scheduled"`, `"operator_command"`, and `"step_started"` with variants. When converting a `runner.PiUpdate`, use `PiName(update.event)`.

31. Run:

    grep -R "name: String" -n src/scherzo/session/event.gleam
    grep -R "publish_lifecycle(.*\"" -n src/scherzo/orchestrator --include='*.gleam' || true
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    The first grep should return no matches. The second grep is a heuristic; inspect any hits and remove lifecycle name string literals from internal publishing. Record the milestone.

32. Create `src/scherzo/tracker/state.gleam` and `src/scherzo/tracker/kind.gleam`, unless `docs/plans/domain-decomposition.md` has already created owner modules where these types should live. In `tracker/kind.gleam`, define `TrackerKind { LinearTracker }` with `to_string` and `from_string`. In `tracker/state.gleam`, define an issue state wrapper:

    pub type IssueState {
      IssueState(raw: String, normalized: String)
    }

    pub fn from_string(value: String) -> Result(IssueState, Nil)
    pub fn to_string(state: IssueState) -> String
    pub fn normalize(state: IssueState) -> String
    pub fn equals_normalized(state: IssueState, expected: IssueState) -> Bool
    pub fn todo() -> IssueState

    If enforcing non-empty states would cause too much decoder churn, make `from_string` return `IssueState(raw: value, normalized: normalized)` for any string and keep existing `issue_has_required_fields` checks. Record the tradeoff.

33. Change `TrackerConfig.kind` from `String` to `TrackerKind`. Update `config.default_tracker_config` and `resolve_tracker` so they parse `"linear"` once and store `LinearTracker`. Keep `error.UnsupportedTrackerKind(other)` for unknown strings.

34. Change `Issue.state` and `BlockerRef.state` from `String` and `Option(String)` to `IssueState` and `Option(IssueState)`. Update Linear decoders, test issue constructors, config active/terminal state parsing, workflow policy, and orchestrator core comparisons to use `tracker/state` helpers. Lists such as `active_states` and `terminal_states` should become `List(IssueState)` if this plan runs before config type decomposition; if config types have already moved, update the owner module there.

35. Replace `normalize(issue.state) == "todo"` in `orchestrator/core.gleam` with a named typed policy. The smallest behavior-preserving change is `issue_state.equals_normalized(issue.state, issue_state.todo())`. If config already has a better owner for blocker policy, introduce `BlockerPolicy(states_requiring_clear_blockers: List(IssueState))` with default `[issue_state.todo()]`, but do not change workflow file syntax in this plan.

36. Add tests in `test/tracker_state_test.gleam` or `test/domain_test.gleam`: parsing `"Todo"` and `" todo "` should produce equal normalized values but preserve the trimmed display string chosen by the implementation; `TrackerKind.from_string("linear")` and `TrackerKind.from_string("LINEAR")` should return `LinearTracker`; unknown tracker kind should produce the same config error as before.

37. Run:

    grep -R "kind: String" -n src/scherzo/domain.gleam src/scherzo/config/types.gleam 2>/dev/null || true
    grep -R "state: String" -n src/scherzo/domain.gleam src/scherzo/tracker/issue.gleam 2>/dev/null || true
    grep -R "== \"todo\"" -n src test --include='*.gleam'
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    The `kind: String`, `state: String`, and `== "todo"` searches should return no internal source matches. Record the milestone.

38. Perform a final cleanup pass. Search for internal comparisons against the typed strings this plan replaced:

    grep -R "artifact.status ==" -n src test --include='*.gleam'
    grep -R "\"worker_down\"\|\"operator_abort\"\|\"stopped\"" -n src/scherzo/orchestrator src/scherzo/session --include='*.gleam'
    grep -R "\"probe_started\"\|\"turn_finished\"\|\"extension_ui_request\"" -n src/scherzo/orchestrator src/scherzo/agent --include='*.gleam'
    grep -R "due_at_ms" -n src test --include='*.gleam'

    Remaining strings should be in conversion functions, tests that assert conversion, external decoders, external encoders, or logs that intentionally name log events. Move any remaining internal branch to typed pattern matching.

39. Run final validation:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

    Update this plan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective. Record the final change with `jj describe -m "Type internal state machine values"`.

## Testing and Falsifiability

This plan is falsified if scheduler behavior still depends on raw status strings. The concrete test is `grep -R "artifact.status == \"success\"" -n src test --include='*.gleam'`, which must return no matches. It is also falsified if `runner.PiUpdate.event` remains a string, `EventPayload.name` remains a string, tracker kind remains a string in source types, or `RetryEntry.due_at_ms` remains after the retry milestone.

Each new ADT must have conversion tests:

- `StepStatus`: `StepSucceeded` converts to `"success"`, `StepFailed` converts to `"failure"`, successful command artifacts are internally `StepSucceeded`, failing or timed-out command artifacts are internally `StepFailed`, and template locals still expose strings.
- `RetryReason`: each variant converts to the exact current log/event string.
- `ParkReason`: `ParkMaxRetryAttempts`, `ParkMaxSessionsPerIssue`, and `ParkOperator("manual")` preserve current strings.
- `WorkerExitReason`: known variants convert to current strings and `Unknown("future")` preserves `"future"` for decoded old/future data.
- `PiEvent`: each known event converts to the current pi event string, unknown pi events preserve the raw string, and daemon event kind classification matches the current test expectations.
- `EventName`: lifecycle variants convert to current EventHub names, pi names delegate to `PiEvent`, and raw names round-trip.
- `TrackerKind`: `"linear"` and `"LINEAR"` parse to `LinearTracker`; unknown values still produce `UnsupportedTrackerKind`.
- `IssueState`: normalization behavior matches current `string.trim |> string.lowercase` behavior used by `orchestrator/core.gleam`, and the blocker policy still treats `Todo` as the state where blockers must be terminal.

Existing tests that must remain green include `test/step_artifact_test.gleam`, `test/workflow_scheduler_test.gleam`, `test/workflow_run_test.gleam`, `test/orchestrator_core_test.gleam`, `test/orchestrator_daemon_test.gleam`, `test/orchestrator_daemon_session_event_test.gleam`, `test/session_event_test.gleam`, `test/control_protocol_test.gleam`, `test/agent_runner_test.gleam`, `test/linear_test.gleam`, and `test/config_test.gleam`.

## Validation and Acceptance

Acceptance requires all of these commands from the repository root to succeed:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    ! grep -R "artifact.status == \"success\"" -n src test --include='*.gleam'
    ! grep -R "event: String" -n src/scherzo/agent/runner.gleam
    ! grep -R "name: String" -n src/scherzo/session/event.gleam
    ! grep -R "due_at_ms" -n src test --include='*.gleam'

If `src/scherzo/domain.gleam` still exists when this plan is implemented, these checks must also succeed:

    ! grep -R "kind: String" -n src/scherzo/domain.gleam
    ! grep -R "state: String" -n src/scherzo/domain.gleam

If `docs/plans/domain-decomposition.md` has already moved those types, run the equivalent checks against `src/scherzo/config/types.gleam` and `src/scherzo/tracker/issue.gleam`.

Behavior acceptance requires external strings to remain stable. EventHub JSON, control protocol JSON, workflow template locals, logs for retry/park/worker exit reasons, and pi event names must match current expectations. Unknown external pi event names and unknown decoded session exit reasons must be preserved rather than rejected.

## Rollout, Recovery, and Idempotence

This is an internal representation refactor. It has no data migration and no operator-facing feature flag. Roll it out as normal code once tests and structural checks pass.

Keep each milestone green and separately described with Jujutsu. If a later broad milestone such as issue state typing causes regressions, revert that milestone while keeping earlier localized improvements like `StepStatus`. Do not leave half-migrated records where both raw strings and typed variants represent the same internal concept.

The steps are safe to repeat. Running grep checks, format, and tests multiple times is safe. Boundary decoders with `Unknown(String)` variants make old or future EventHub/control data safe to read after the change.

## Artifacts and Notes

Current examples verified during plan authoring:

    src/scherzo/domain.gleam: Issue.state is String.
    src/scherzo/domain.gleam: TrackerConfig.kind is String.
    src/scherzo/domain.gleam: ParkedEntry.reason is String.
    src/scherzo/domain.gleam: RetryEntry.due_at_ms is populated from delay_ms.
    src/scherzo/session/event.gleam: EventPayload.name is String and Exited carries String.
    src/scherzo/agent/runner.gleam: PiUpdate.event is String.
    src/scherzo/step_artifact.gleam: StepArtifact.status is String.
    src/scherzo/workflow_scheduler.gleam: mark_finished branches on artifact.status == "success".
    src/scherzo/workflow_run.gleam: is_fatal_result branches on artifact.status == "success".
    src/scherzo/orchestrator/daemon.gleam: YAML command step session finish reason branches on artifact.status == "success".
    src/scherzo/orchestrator/core.gleam: blockers_satisfied checks normalize(issue.state) == "todo".

## Interfaces and Dependencies

The exact module names may shift if the domain decomposition plan lands first, but the intended public shapes are:

In `src/scherzo/step_artifact.gleam`:

    pub type StepStatus {
      StepSucceeded
      StepFailed
    }

    pub fn status_to_string(status: StepStatus) -> String
    pub fn status_from_exit(exit_code: Int, timed_out: Bool) -> StepStatus
    pub fn succeeded(status: StepStatus) -> Bool

In `src/scherzo/orchestrator/reason.gleam`:

    pub type RetryReason {
      RetryAfterFailure
      RetryAfterContinuation
      RetryNoSlots
      RetryPollFailed
    }

    pub type ParkReason {
      ParkMaxRetryAttempts
      ParkMaxSessionsPerIssue
      ParkOperator(reason: String)
    }

    pub type StopReason {
      StopOperatorAbort
      StopAfterCurrentTurn
      StopNoAvailableSlot
      StopCustom(reason: String)
    }

In `src/scherzo/session/reason.gleam` or `src/scherzo/session/event.gleam`:

    pub type WorkerExitReason {
      Normal
      Failed
      OperatorAbort
      WorkerDown
      Stopped
      Unknown(reason: String)
    }

In `src/scherzo/agent/pi_event.gleam`:

    pub type PiEvent {
      ProbeStarted
      ProbeFinished
      PiSessionStarted
      TurnFinished
      MessageStart
      MessageUpdate
      MessageEnd
      ToolExecutionStart
      ToolExecutionUpdate
      ToolExecutionEnd
      Message
      ExtensionUiRequest
      ExtensionUiResponse
      AgentStart
      TurnStart
      TurnEnd
      AgentEnd
      UnknownPiEvent(name: String)
    }

In `src/scherzo/session/event.gleam`:

    pub type LifecycleEventName {
      DispatchStarted
      WorkerStarted
      StopRequested
      WorkerExited
      WorkerDown
      RetryScheduled
      OperatorCommand
      StepStarted
      CustomLifecycleEvent(name: String)
    }

    pub type EventName {
      LifecycleName(LifecycleEventName)
      PiName(pi_event.PiEvent)
      RawEventName(name: String)
    }

In `src/scherzo/tracker/kind.gleam`:

    pub type TrackerKind {
      LinearTracker
    }

In `src/scherzo/tracker/state.gleam`:

    pub type IssueState {
      IssueState(raw: String, normalized: String)
    }

    pub fn from_string(value: String) -> Result(IssueState, Nil)
    pub fn to_string(state: IssueState) -> String
    pub fn normalize(state: IssueState) -> String
    pub fn todo() -> IssueState
    pub fn equals_normalized(state: IssueState, expected: IssueState) -> Bool
