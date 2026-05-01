# Replace stringly internal state machine values with typed variants

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo's internal state machines should use Gleam custom types for states, statuses, event names, retry reasons, park reasons, and worker exit reasons. Strings should exist only at actual integration boundaries such as Linear API payloads, pi RPC JSON, control API JSON, durable state ledger and snapshot JSON, logs, templates, and terminal output. Those boundary strings should be parsed into internal types once and converted back to strings only when leaving the typed core.

The visible proof is that typos like `"sucess"`, `"retry poll failed"`, or `"worker_dwon"` cannot silently change scheduler behavior. The step scheduler should branch on `StepSucceeded` or `StepFailed`, not on `artifact.status == "success"`. The daemon should publish `WorkerExited(Normal)` or `WorkerExited(Failed)`, not hand around raw strings. `runner.PiUpdate` should carry a `PiEvent` variant for both pi RPC events and runner-generated lifecycle updates, and `src/scherzo/orchestrator/event_publisher.gleam` should classify events by pattern matching over known variants. Tracker kind should be `LinearTracker`, not the string `"linear"`. The retry entry field currently named `due_at_ms` should stop lying everywhere in this repository: it stores a delay, so it should be renamed to `delay_ms` in runtime types, durable ledger records, projection snapshots, JSON fields, and tests.

## Problem Framing and Constraints

Scherzo is mostly a state machine. It polls tracker issues, schedules retries, starts workers, handles worker exits, runs workflow DAG steps, publishes EventHub events, and accepts operator commands. In the current tree many of those internal transitions are keyed by strings. The current `src/scherzo/domain.gleam` stores `Issue.state` as `String`, `BlockerRef.state` as `Option(String)`, `TrackerConfig.kind` as `String`, `TrackerConfig.active_states` and `terminal_states` as `List(String)`, `AgentConfig.max_concurrent_agents_by_state` as `Dict(String, Int)`, `ParkedEntry.reason` as `String`, and `RetryEntry.due_at_ms` even though `src/scherzo/orchestrator/core.gleam` fills that field with a delay. `src/scherzo/state/record.gleam` and `src/scherzo/state/projection.gleam` also encode and decode retry fields named `due_at_ms`, even though they represent the same delay. `src/scherzo/session/event.gleam` stores `EventPayload.name` as `String` and `SessionStatus.Exited(reason: String)`. `src/scherzo/agent/runner.gleam` stores `PiUpdate.event` as `String`. `src/scherzo/step_artifact.gleam` stores `StepArtifact.status` as `"success"` or `"failure"`. `src/scherzo/workflow_scheduler.gleam`, `src/scherzo/workflow_run.gleam`, and `src/scherzo/orchestrator/daemon.gleam` branch on that status string. `src/scherzo/orchestrator/event_publisher.gleam` classifies `runner.PiUpdate` values by raw event-name strings. `src/scherzo/orchestrator/core.gleam` hard-codes the blocker policy with `normalize(issue.state) == "todo"` and schedules retries with raw reason strings such as `"failure"`, `"continuation"`, `"retry poll failed"`, and `"no available orchestrator slots"`. `src/scherzo/orchestrator/core.gleam` and `src/scherzo/orchestrator/daemon.gleam` also normalize issue-state strings for active, terminal, blocker, and per-state dispatch-limit checks.

This matters because string typos become behavior changes exactly where correctness matters most. A misspelled step status changes whether a DAG continues. A misspelled retry reason changes logs and events. A raw issue state string makes it hard to tell whether a function expects a user-visible Linear state, a normalized state key, or a config state. A raw pi event string makes EventHub classification non-exhaustive.

Backward compatibility is not a constraint for this plan. It may change control JSON, durable state ledger and projection snapshot JSON, test fixtures, and operator-facing string fields where that makes the typed design smaller and clearer. Existing local `.scherzo-state/ledger` data may be deleted or regenerated after this change. Keep boundary string conversions only where Scherzo still talks to external systems or presents text to users, not to preserve old schemas.

## Strategy Overview

Migrate from the smallest and safest stringly values to the broadest values. Start with `StepStatus`, because it is localized and directly controls workflow scheduling. Then introduce typed retry, park, and worker exit reasons because those strings drive daemon/session lifecycle behavior. Then type pi events and EventHub event names. Move tracker kind and issue state later because they touch config parsing, Linear decoding, workflow policy, blockers, and many tests.

Use explicit conversion modules. Every new type should have a small set of functions such as `to_string`, `from_string`, `is_success`, or `normalize`. String conversion belongs at boundaries: config parsing, Linear decoding, pi RPC decoding, EventHub/control JSON decoding and encoding, logging, and template locals. Internal branches should pattern match on variants.

This plan must remain self-contained even if the repository layout changes. If the current checkout has already moved types out of `src/scherzo/domain.gleam`, use the new owner modules that actually contain `Issue`, `TrackerConfig`, `AgentConfig`, `ParkedEntry`, and `RetryEntry`; otherwise introduce the typed modules in the current tree and update `domain.gleam` fields directly. Do not read or depend on another plan for the required semantics: this document defines the type names, conversions, boundaries, and validation commands. Do not use this work as an excuse to split all of `domain.gleam`; that is a separate refactor.

## Alternatives Considered

One alternative is to add constants for strings, such as `const step_success = "success"`. That reduces typo risk in one file but still gives no exhaustiveness checking and still lets any arbitrary string represent a scheduler state.

Another alternative is to type every string in the repository at once, including command protocol reasons, Linear state names, tool statuses, pi RPC record types, log event names, and errors. That is too broad. Many strings are true integration-boundary strings, and changing all of them in one pass would make regressions hard to isolate.

A third alternative is to enumerate every possible Linear issue state as variants. That is not correct because Linear states are configured by the user's workspace. The right internal type for issue state is a parsed wrapper around the external state name with normalization helpers, not a closed enum of all possible state names.

A fourth alternative is to fix `RetryEntry.due_at_ms` by computing absolute due timestamps everywhere. That might be useful later, but the current value stores only a delay in runtime state, ledger records, and projection snapshots. Renaming the field to `delay_ms` across all repository-owned shapes is the smallest truthful fix. If future scheduling needs absolute due time, add a separate `due_at_ms` field then and populate it from a clock.

## Risks and Countermeasures

The main behavior risk is changing serialized strings. Countermeasure: each typed value must have a `to_string` function that returns the exact current strings, and existing JSON/protocol/template tests must continue to pass. Add direct conversion tests for every new type.

The main persistence risk is that old local ledger and projection files will no longer decode after the `due_at_ms` to `delay_ms` rename. Countermeasure: accept this as an intentional breaking change, update ledger/projection schema tests to the new field name, and document rollout as deleting or regenerating local `.scherzo-state/ledger` data after deployment.

The main compile-risk is large constructor churn. Countermeasure: migrate one type family at a time and run format/tests after each milestone. Do not combine `StepStatus`, `PiEvent`, and `IssueState` in one commit.

The main design risk is over-typing values that are truly open-ended. Countermeasure: use closed ADTs only for closed internal vocabularies such as step status, tracker kind, retry reason, park reason, and known worker exit reasons. For open external vocabularies such as pi RPC event types, use an explicit unknown variant only when Scherzo must keep running with external input it does not control. For Linear issue states, use a typed wrapper that keeps the original state string and normalized key.

The main import-cycle risk appears if session/event types import orchestrator reason types. Countermeasure: put session exit reason types in the session-owned module `src/scherzo/session/reason.gleam`. Orchestrator code may import session reason types; session modules must not import orchestrator modules. Likewise, `src/scherzo/agent/pi_event.gleam` must not import `runner` or `pi_rpc`; it should be a small independent conversion module that both runner and event publisher can import.

The main UX risk is losing user-supplied park reasons. Countermeasure: model park reasons as an ADT that includes `ParkOperator(String)` for user/operator-supplied text. Only known internal reasons become closed variants; user-entered reason text remains preserved and encoded at logging/control boundaries.

## Progress

- [x] (2026-04-30 10:47Z) Verified the current test baseline earlier in this work session with `direnv exec . gleam test`; it passed with `377 passed, no failures`.
- [x] (2026-04-30 11:45Z) Fact-checked current stringly examples in `src/scherzo/domain.gleam`, `src/scherzo/session/event.gleam`, `src/scherzo/agent/runner.gleam`, `src/scherzo/step_artifact.gleam`, `src/scherzo/workflow_scheduler.gleam`, `src/scherzo/workflow_run.gleam`, `src/scherzo/orchestrator/daemon.gleam`, and `src/scherzo/orchestrator/core.gleam`.
- [x] (2026-04-30 11:55Z) Drafted this plan with milestones ordered from localized status strings to broader tracker issue state typing.
- [x] (2026-05-01 03:47Z) Reviewed the plan against the current tree and amended it to identify durable `due_at_ms` JSON fields, include the actual `event_publisher` event classification owner, cover runner-generated pi lifecycle event names, and include per-state dispatch-limit state keys.
- [x] (2026-05-01 04:06Z) Removed backward-compatibility constraints from the plan, simplified durable state handling to rename `due_at_ms` to `delay_ms` everywhere, and aligned rollout guidance with deleting or regenerating old local ledger data.
- [x] (2026-05-01 04:35Z) Implemented typed `StepStatus` in `src/scherzo/step_artifact.gleam`, updated workflow scheduling/daemon branches to use the typed success predicate, and kept template locals string-compatible at the boundary.
- [x] (2026-05-01 05:10Z) Added typed orchestrator retry, park, and stop reasons in `src/scherzo/orchestrator/reason.gleam`; changed effect and parked-entry internals to carry variants; and renamed retry delay fields from `due_at_ms` to `delay_ms` in runtime, durable records, projections, JSON, and tests.
- [x] (2026-05-01 05:40Z) Added typed session worker exit reasons in `src/scherzo/session/reason.gleam`, changed session status and `hub.finish_session` to use variants, and converted to strings only in session JSON, control protocol, rendering, logs, and lifecycle message boundaries.
- [x] (2026-05-01 06:05Z) Added typed `PiEvent` in `src/scherzo/agent/pi_event.gleam`, changed `runner.PiUpdate.event` to the ADT, preserved unknown external pi record types with `UnknownPiEvent`, and updated EventHub classification to pattern match on variants.
- [x] (2026-05-01 06:25Z) Added typed lifecycle/pi event names in `src/scherzo/session/event.gleam`, changed `EventPayload.name` to `EventName`, and moved string conversion into EventHub JSON/control/rendering boundaries.
- [x] (2026-05-01 06:55Z) Added `src/scherzo/tracker/kind.gleam` and `src/scherzo/tracker/state.gleam`; changed tracker kind, issue state, blocker state, active/terminal state lists, and per-state dispatch-limit keys to typed values; and updated Linear/config/orchestrator/runner/service/workflow/test fixtures accordingly.
- [x] (2026-05-01 07:10Z) Ran structural checks for removed raw status/event/durable fields and final validation with `direnv exec . gleam format --check src test` and `direnv exec . gleam test`; the suite passed with `467 passed, no failures`.
- [x] (2026-05-01 07:40Z) Removed the leftover durable-state compatibility decode path for old retry delay fields. Ledger records and projection snapshots now require `delay_ms` for scheduled retries; old local data using the previous field name is rejected rather than migrated. Re-ran format and tests; the suite passed with `471 passed, no failures`.
- [x] Milestone 0: add characterization tests for current external string encodings.
- [x] Milestone 1: introduce `StepStatus` and remove internal step status string comparisons.
- [x] Milestone 2: introduce typed retry reasons, park reasons, worker stop reasons, and rename retry delay storage.
- [x] Milestone 3: introduce typed session/worker exit reasons in EventHub session status.
- [x] Milestone 4: introduce `PiEvent` for `runner.PiUpdate.event` and update EventHub event classification.
- [x] Milestone 5: introduce typed EventHub event names for lifecycle and pi events.
- [x] Milestone 6: introduce typed tracker kind and issue state wrappers.
- [x] Milestone 7: remove remaining internal string comparisons and write the retrospective.

## Surprises & Discoveries

- Observation: `due_at_ms` appears in both runtime state and durable state modules, and all occurrences describe the same retry delay concept.
  Evidence: a search for `due_at_ms` found `src/scherzo/domain.gleam` and `src/scherzo/orchestrator/core.gleam`, plus `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, and state ledger/projection tests. Because this repository is not preserving backward compatibility right now, this plan renames all of those fields to `delay_ms` together.

- Observation: Command status is already partially typed.
  Evidence: `src/scherzo/control/command.gleam` defines `CommandStatus` with variants `Applied`, `Queued`, `Rejected(reason)`, `NotFound`, and `NotAllowed(reason)`, plus boundary functions `status_to_string` and `status_from_string`. This plan should copy that pattern rather than replacing it.

- Observation: Some string values are intentionally open external vocabularies.
  Evidence: `src/scherzo/agent/pi_rpc.gleam` decodes `RpcRecord.type_` from pi RPC JSON, and `tool_status` may come from pi/tool payloads. This plan keeps `UnknownPiEvent(String)` because pi RPC is external input Scherzo does not control; this is resilience for current operation, not a backward-compatibility promise.

- Observation: pi event classification lives in `src/scherzo/orchestrator/event_publisher.gleam`, not primarily in the daemon.
  Evidence: `event_publisher.kind_for_update`, `status_for_update`, and `pi_type_for_update` branch on `update.event`; daemon code calls `event_publisher.worker_update` and only logs or publishes lifecycle events directly.

- Observation: `runner.PiUpdate.event` carries more than raw pi RPC record types.
  Evidence: `src/scherzo/agent/runner.gleam` creates runner lifecycle updates such as `"operator_prompt_sent"`, `"operator_prompt_queued"`, `"operator_prompt_dropped"`, `"operator_ui_timeout"`, `"pi_abort_sent"`, and `"pi_abort_failed"` in addition to pi RPC names such as `"message_start"` and `"extension_ui_request"`.

- Observation: issue-state strings also key per-state dispatch limits.
  Evidence: `src/scherzo/domain.gleam` stores `AgentConfig.max_concurrent_agents_by_state: Dict(String, Int)`, and `src/scherzo/orchestrator/core.gleam` plus `src/scherzo/orchestrator/daemon.gleam` normalize issue states before looking up per-state limits.

- Observation: Broad mechanical test-fixture conversion can accidentally rewrite YAML/text fixtures if it treats source text and string literal contents the same way.
  Evidence: while converting tracker-state fixtures, YAML strings such as `active_states: [Todo]` were temporarily rewritten as Gleam expressions. The broken fixtures were restored and the final suite validates config parsing through real YAML strings.

- Observation: The acceptance grep `name: String` can match function parameters, not just record fields.
  Evidence: after `EventPayload.name` was typed, `pub fn name_from_string(name: String)` still matched the heuristic. Renaming the parameter to `value` made the structural check accurately reflect the field migration.

## Decision Log

- Decision: Start with step artifact status.
  Rationale: It is a small, high-value change. The current scheduler branches on `artifact.status == "success"`, so this milestone proves the pattern before broader migrations.
  Date: 2026-04-30

- Decision: Put conversions at current integration and presentation boundaries, but do not preserve old schemas for compatibility.
  Rationale: The repository is still moving quickly. Clean typed shapes are more valuable right now than accepting old control JSON, old EventHub JSON, or old local ledger files.
  Date: 2026-05-01

- Decision: Use a typed wrapper for Linear issue state rather than enumerating states.
  Rationale: Linear state names are workspace-specific and configurable. A closed enum would be wrong; the internal type should distinguish an issue state from arbitrary text while keeping the configured display value.
  Date: 2026-04-30

- Decision: Rename every repository-owned retry delay field from `due_at_ms` to `delay_ms` rather than computing an absolute due time in this plan.
  Rationale: Runtime state, ledger records, and projection snapshots all currently store a delay. Because backward compatibility is not a constraint, one repository-wide rename is simpler than preserving old durable field names.
  Date: 2026-05-01

- Decision: Accept breaking local durable-state changes.
  Rationale: The local durable ledger is not yet a startup recovery backend. Operators can delete or regenerate `.scherzo-state/ledger` data after this refactor instead of carrying a migration solely for old development snapshots.
  Date: 2026-05-01

- Decision: Type all `runner.PiUpdate.event` values that Scherzo emits intentionally, not only raw pi RPC record names.
  Rationale: `PiUpdate` is used for pi RPC records and for runner lifecycle events. Leaving lifecycle names as ad-hoc strings would preserve a typo path in event classification and logs.
  Date: 2026-05-01

- Decision: Include per-state dispatch-limit keys in issue-state typing.
  Rationale: `max_concurrent_agents_by_state` controls scheduling behavior from normalized state strings. It is part of the same issue-state policy surface as active, terminal, and blocker checks.
  Date: 2026-05-01

- Decision: Decode unknown EventHub event names as `PiName(UnknownPiEvent(raw))` rather than failing the whole session event decode.
  Rationale: EventHub payload names include pi RPC record names, and pi RPC is an open external vocabulary. Preserving unknown names as explicit pi unknowns keeps control/attach clients resilient while lifecycle names still use closed variants when Scherzo emits them.
  Date: 2026-05-01

## Outcomes & Retrospective

The refactor is complete. Internal scheduler/status/reason/event/tracker state now uses typed Gleam values rather than raw strings. Strings are still used at integration and presentation boundaries: YAML config, Linear GraphQL request/response payloads, pi RPC JSON, EventHub/control JSON, durable ledger/projection JSON, templates, logs, and terminal output.

Completed typed surfaces:

- `StepArtifact.status` is `StepStatus`, with scheduler/runner branches using `step_artifact.succeeded` rather than string equality.
- Orchestrator retry, park, and stop effects use `RetryReason`, `ParkReason`, and `StopReason`; `ParkedEntry.reason` is typed.
- Retry delay storage is consistently named `delay_ms` across runtime records, durable ledger records, projection snapshots, JSON fields, and tests; durable decoders do not accept old retry-delay field aliases.
- Session exit status and `hub.finish_session` use `WorkerExitReason` variants.
- `runner.PiUpdate.event` uses `PiEvent`, including known runner lifecycle variants and `UnknownPiEvent(raw)` for forward-compatible pi RPC input.
- `EventPayload.name` uses `EventName`, with lifecycle names and pi names converted only at JSON/control/rendering boundaries.
- Tracker kind is `TrackerKind`, issue and blocker states are `IssueState`, active/terminal states are `List(IssueState)`, and per-state dispatch limits are keyed by `IssueStateKey`.

Final validation performed:

- `direnv exec . gleam format --check src test`
- `direnv exec . gleam test` (`471 passed, no failures`)
- Structural checks returned no matches for `artifact.status == "success"`, `event: String` in `runner.gleam`, `name: String` in `session/event.gleam`, `due_at_ms` in source/tests, `kind: String` in `domain.gleam`, `state: String` in `domain.gleam`, `max_concurrent_agents_by_state: Dict(String` in `domain.gleam`, and `normalize_state` in orchestrator source. Searches for `== "todo"` have no source matches; remaining test matches assert issue-state key conversion behavior.

The only important caveat is intentional: old local durable state using `due_at_ms` is not migrated. Delete or regenerate old `.scherzo-state/ledger` data before depending on it with this branch.

## Context and Orientation

The current central type module is `src/scherzo/domain.gleam`. It defines `Issue` with `state: String`, `BlockerRef` with `state: Option(String)`, `TrackerConfig` with `kind: String`, `active_states: List(String)`, and `terminal_states: List(String)`, `AgentConfig` with `max_concurrent_agents_by_state: Dict(String, Int)`, `ParkedEntry` with `reason: String`, and `RetryEntry(issue_id, due_at_ms, timer_generation)`. The `RetryEntry.due_at_ms` field is set from a delay in `src/scherzo/orchestrator/core.gleam` inside `schedule_retry`. `src/scherzo/state/record.gleam` and `src/scherzo/state/projection.gleam` separately encode durable retry records and projection snapshots with a JSON field named `due_at_ms`; this plan renames those to `delay_ms` too.

Step artifacts are built in `src/scherzo/step_artifact.gleam`. `StepArtifact.status` is currently a `String`. `from_agent_success` sets it to `"success"`. `from_command_result_with_truncation` sets it to `"success"` or `"failure"`. `artifact_locals` exposes that value to workflow templates as `steps.<step_id>.status`.

Workflow scheduling is implemented in `src/scherzo/workflow_scheduler.gleam` and `src/scherzo/workflow_run.gleam`. Both currently check `artifact.status == "success"` to decide whether a step succeeded, failed-but-continued, or failed fatally.

Pi update events are created in `src/scherzo/agent/runner.gleam`. `PiUpdate.event` is currently a `String`. Known names include pi and tool events such as `"probe_started"`, `"probe_finished"`, `"pi_session_started"`, `"turn_finished"`, `"message_start"`, `"message_update"`, `"message_end"`, `"tool_execution_start"`, `"tool_execution_update"`, `"tool_execution_end"`, `"message"`, `"extension_ui_request"`, `"extension_ui_response"`, `"agent_start"`, `"turn_start"`, `"turn_end"`, and `"agent_end"`. They also include runner-generated lifecycle names such as `"operator_prompt_sent"`, `"operator_prompt_queued"`, `"operator_prompt_dropped"`, `"operator_ui_timeout"`, `"pi_abort_sent"`, and `"pi_abort_failed"`. Unknown pi RPC record types are represented as raw pi events because pi RPC is external input.

EventHub session events are defined in `src/scherzo/session/event.gleam`. `EventPayload.kind` is already typed as `EventKind`, but `EventPayload.name` is a `String`. Session exit status is `Exited(reason: String)`. JSON conversion lives in `src/scherzo/session/json.gleam`, control protocol decoding/encoding lives in `src/scherzo/control/protocol.gleam`, and terminal rendering uses `src/scherzo/terminal/render.gleam`.

EventHub update classification happens in `src/scherzo/orchestrator/event_publisher.gleam`. It passes `PiUpdate` values through `kind_for_update`, `status_for_update`, and `pi_type_for_update`, and those functions currently compare raw event strings. Daemon lifecycle publishing happens mostly in `src/scherzo/orchestrator/daemon.gleam`. It passes lifecycle names such as `"dispatch_started"`, `"worker_started"`, `"stop_requested"`, `"worker_exited"`, `"worker_down"`, `"retry_scheduled"`, `"operator_command"`, and `"step_started"`, and exit reasons such as `"normal"`, `"failed"`, `"operator_abort"`, `"operator_stop_after_current_turn"`, `"worker_down"`, and `"stopped"`.

## Preconditions and Verified Facts

Before implementing, run from the repository root:

    jj status
    direnv exec . gleam test

At plan-authoring time, the test suite ended with:

    377 passed, no failures

Re-run these searches before coding because the tree may have changed:

    grep -R "artifact.status == \"success\"" -n src test --include='*.gleam'
    grep -R "artifact.status" -n test/command_step_test.gleam test/step_artifact_test.gleam
    grep -R "event: String" -n src/scherzo/agent/runner.gleam
    grep -R "update.event" -n src/scherzo/orchestrator src/scherzo/agent --include='*.gleam'
    grep -R "name: String" -n src/scherzo/session/event.gleam
    grep -R "kind: String" -n src/scherzo/domain.gleam src/scherzo/config 2>/dev/null || true
    grep -R "max_concurrent_agents_by_state" -n src test --include='*.gleam'
    grep -R "due_at_ms" -n src test --include='*.gleam'

At plan-review time, the status comparisons appeared in `src/scherzo/workflow_scheduler.gleam`, `src/scherzo/workflow_run.gleam`, and `src/scherzo/orchestrator/daemon.gleam`, with test assertions in `test/command_step_test.gleam` and `test/step_artifact_test.gleam`; `PiUpdate.event` was a string in `src/scherzo/agent/runner.gleam`; `EventPayload.name` was a string in `src/scherzo/session/event.gleam`; `event_publisher.kind_for_update`, `status_for_update`, and `pi_type_for_update` compared `update.event` strings; tracker kind and state config were strings in `src/scherzo/domain.gleam`; `max_concurrent_agents_by_state` used normalized string keys; and `due_at_ms` appeared both in the in-memory runtime retry entry and in durable state ledger/projection code.

This repository uses Jujutsu. Do not use mutating `git` commands. Use `jj status` for status, `jj describe -m "message"` to describe the current change, and `jj new` to start a new change after a green milestone.

## Scope Boundaries

In scope: `StepStatus`, `RetryReason`, `ParkReason`, `StopReason` where it is emitted by `core.Effect.StopWorker`, `WorkerExitReason`, `PiEvent`, lifecycle event names, tracker kind, issue state wrappers, per-state dispatch-limit keys, the blocker policy's `"todo"` comparison, and the in-memory runtime retry delay field rename.

Out of scope: changing Linear state configuration syntax beyond parsing configured states into wrappers; changing pi RPC wire strings that Scherzo must send to pi; changing workflow template variable names unless required by a typed boundary; changing the retry algorithm; changing the daemon actor structure.

It is acceptable to make breaking changes to repository-owned JSON, control protocol payloads, durable state schemas, and tests when that makes the typed design clearer. It is not acceptable for internal scheduler branches to compare raw strings once the typed value exists.

## Milestones

Milestone 0 adds characterization tests for current behavior that should remain semantically equivalent. At the end, tests prove step artifact template status behavior, command-step artifact status behavior, session exit reason encoding, pi event naming, command result status behavior, retry delay ledger/projection behavior, and tracker kind parsing. These tests may be updated during later milestones to the new clean field names and typed values.

Milestone 1 introduces `StepStatus`. At the end, `StepArtifact.status` is a custom type, and no source file decides step success by comparing status to `"success"`.

Milestone 2 introduces typed orchestrator reasons and fixes retry delay naming everywhere. At the end, `core.ScheduleRetry`, `core.ParkIssue`, and `domain.ParkedEntry.reason` or its moved owner use typed reasons. `core.StopWorker` uses typed stop reasons for the existing `"terminal"` and `"non_active"` paths. Logs and EventHub messages convert reasons with `to_string`. Runtime `RetryEntry.due_at_ms`, durable state ledger records, projection snapshots, JSON fields, and tests are all renamed to `delay_ms`.

Milestone 3 introduces typed session exit reasons. At the end, `hub.finish_session` and `SessionStatus.Exited` accept a typed reason, while EventHub JSON still emits the same `exit_reason` strings.

Milestone 4 introduces typed pi events. At the end, `runner.PiUpdate.event` is a `PiEvent`, runner-created lifecycle updates use variants instead of ad-hoc strings, `src/scherzo/orchestrator/event_publisher.gleam` event classification pattern matches on variants, daemon logging converts events through `pi_event.to_string`, and unknown pi RPC event types are represented with `UnknownPiEvent(raw)` because pi RPC is external input.

Milestone 5 introduces typed EventHub event names. At the end, `EventPayload.name` is an event-name ADT and JSON/protocol conversion is the only place that turns it into or from a string.

Milestone 6 introduces typed tracker kind, issue state wrappers, and typed normalized state keys. At the end, config parsing parses tracker kind to `LinearTracker`, issue state strings from Linear/config are represented by an `IssueState` wrapper, per-state dispatch limits use `IssueStateKey`, state normalization lives in one module, and the blocker `"todo"` policy is represented by a named typed value.

Milestone 7 removes leftover internal string comparisons and records the outcome.

## Plan of Work

Start by adding tests for boundary strings so the refactor can be aggressive internally without accidentally changing output. Then change step status because it is local. Next change reasons because they are mostly inside orchestrator and session lifecycle paths. Then change pi events and EventHub names because they affect session JSON and control protocol. Finally change tracker kind and issue state because they touch config parsing, Linear decoding, workflow policy, and orchestrator dispatch.

For each type, create the type and conversion helpers first. Update internal records to store the type. Update all compile errors by replacing string constructors with variants. Update boundary code to call `to_string` or `from_string`. When touching durable ledger or snapshot modules, prefer the clean typed shape and new field names over compatibility shims. Run grep checks that prove old internal comparisons are gone. Run format and tests. Commit only when green.

## Concrete Steps

1. From the repository root, run `jj status`. If unrelated source changes exist, stop and either move to a clean workspace or record them in this plan.

2. Run `direnv exec . gleam test`. Expect the suite to end with `no failures`. The count was `377 passed` at plan authoring.

3. Add or update characterization tests before code changes. In `test/step_artifact_test.gleam`, assert that a successful command step still exposes template local `steps.<id>.status` as `"success"` and a failing command still exposes `"failure"`. In `test/command_step_test.gleam`, keep the current direct artifact status string assertions for this red/green baseline; after `StepStatus` exists in Milestone 1, change those assertions to compare `step_artifact.status_to_string(artifact.status)` with `"success"` or `"failure"`. In `test/session_event_test.gleam` or `test/control_protocol_test.gleam`, assert the current session-exit reason behavior before replacing it with typed values. In `test/agent_runner_test.gleam`, `test/orchestrator_event_publisher_test.gleam`, or an added `test/pi_event_test.gleam`, assert known pi event names convert to their current strings. In `test/state_record_test.gleam` and `test/state_projection_test.gleam`, assert current retry delay JSON behavior before renaming the field to `"delay_ms"` in Milestone 2.

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
      StopTerminal
      StopNonActive
    }

    Add `retry_to_string`, `park_to_string`, and `stop_to_string` with the strings used by the new boundaries. `stop_to_string(StopTerminal)` should return `"terminal"`, and `stop_to_string(StopNonActive)` should return `"non_active"`. Operator abort and stop-after-current-turn are session exit or park reasons handled in later daemon/session steps, not current `core.StopWorker` reasons.

11. In `src/scherzo/orchestrator/core.gleam`, change `core.Effect.ScheduleRetry.reason` from `String` to `reason.RetryReason`, `core.Effect.ParkIssue.reason` from `String` to `reason.ParkReason`, and `core.Effect.StopWorker.reason` from `String` to `reason.StopReason` for the existing `StopWorker(refreshed.id, "terminal")` and `StopWorker(refreshed.id, "non_active")` call sites. If `StopWorker` has grown new call sites by implementation time, add explicit variants for each closed internal reason; do not add a catch-all stop reason just for compatibility.

12. Update `schedule_retry` to accept `RetryReason`. Replace calls: worker failure uses `RetryAfterFailure`, continuation uses `RetryAfterContinuation`, retry poll failure uses `RetryPollFailed`, and no-slot scheduling uses `RetryNoSlots`.

13. Change `ParkedEntry.reason` in `src/scherzo/domain.gleam` or its moved owner to `reason.ParkReason`. Replace internal park calls: max retries uses `ParkMaxRetryAttempts`, max sessions uses `ParkMaxSessionsPerIssue`, operator park commands convert user text to `ParkOperator(reason)` at the command boundary, and daemon paths that park because a session was operator-aborted or stopped-after-current-turn store `ParkOperator(reason)` because those reasons come from operator/session control paths.

14. Rename `RetryEntry.due_at_ms` to `delay_ms` in `src/scherzo/domain.gleam` or its moved owner, and update `core.schedule_retry` to set `delay_ms: delay_ms`. Also rename retry delay fields in `src/scherzo/state/record.gleam`, `src/scherzo/state/projection.gleam`, and their JSON tests from `due_at_ms` to `delay_ms`. Do not add decode aliases for the old field name.

15. Update daemon and service logging to call `reason.retry_to_string`, `reason.park_to_string`, or `reason.stop_to_string` when producing log fields or EventHub message text. Keep log strings unchanged.

16. Update `test/domain_test.gleam` or `test/orchestrator_core_test.gleam` so parked issue assertions compare `reason.ParkMaxRetryAttempts` as a value and separately assert `reason.park_to_string(reason.ParkMaxRetryAttempts) == "max_retry_attempts"`. Add an orchestrator core test that the runtime retry entry stores `delay_ms`, not `due_at_ms`. Update `test/state_record_test.gleam`, `test/state_projection_test.gleam`, `test/state_ledger_test.gleam`, and `test/state_compaction_test.gleam` to assert durable `delay_ms` JSON and projection fields.

17. Run:

    grep -R "due_at_ms" -n src test --include='*.gleam'
    grep -R "\"retry poll failed\"\|\"continuation\"\|\"max_retry_attempts\"\|\"terminal\"\|\"non_active\"" -n src/scherzo/orchestrator src/scherzo/domain.gleam 2>/dev/null || true
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    The `due_at_ms` grep should return no matches anywhere in source or tests. Remaining reason strings should live only in conversion functions, boundary serialization, or tests that assert conversion. Record the milestone.

18. Create `src/scherzo/session/reason.gleam`. Keep this type in a session-owned module because daemon, session JSON, control protocol, and terminal code all need to convert exit reasons without introducing imports from session modules back into orchestrator modules. Define:

    pub type WorkerExitReason {
      Normal
      Failed
      OperatorAbort
      OperatorStopAfterCurrentTurn
      WorkerDown
      Stopped
    }

    pub fn to_string(reason: WorkerExitReason) -> String
    pub fn from_string(reason: String) -> Result(WorkerExitReason, Nil)

    `to_string` should emit `"normal"`, `"failed"`, `"operator_abort"`, `"operator_stop_after_current_turn"`, `"worker_down"`, and `"stopped"`. `from_string` should reject unknown reason strings instead of preserving them.

19. Change `session/event.gleam` so `SessionStatus.Exited` carries `WorkerExitReason` instead of `String`. Change `exit_reason` to return `Option(WorkerExitReason)`, and update JSON/protocol code to call `session_reason.to_string` when it needs the boundary string. Keep `status_to_string(Exited(_)) == "exited"`.

20. Change `src/scherzo/session/hub.gleam` so `finish_session` accepts `WorkerExitReason`. Update all daemon, workflow step, and protocol call sites to pass variants. For decoded external protocol data, use `from_string`; reject unknown values with the existing protocol/config error path rather than carrying them forward.

21. Update `src/scherzo/session/json.gleam`, `src/scherzo/control/protocol.gleam`, and terminal rendering to call `session_reason.to_string` when emitting strings. Add tests that unknown decoded exit reasons are rejected and known reasons encode to the chosen current strings.

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
      OperatorPromptSent
      OperatorPromptQueued
      OperatorPromptDropped
      OperatorUiTimeout
      PiAbortSent
      PiAbortFailed
      UnknownPiEvent(name: String)
    }

    Add `to_string`, `from_string`, `is_message_update`, `is_blocking_ui_request`, `is_token_stats`, and any small predicates needed by `src/scherzo/orchestrator/event_publisher.gleam` classification. Every runner-generated lifecycle name listed here must parse to a specific variant; reserve `UnknownPiEvent(name)` for pi RPC names that Scherzo does not know yet.

24. Change `runner.PiUpdate.event` in `src/scherzo/agent/runner.gleam` from `String` to `pi_event.PiEvent`. Update `lifecycle_update`, `lifecycle_update_with_message`, `lifecycle_update_with_request`, `pi_session_started_update`, `token_update`, and `update_from_record` to accept or construct variants. Known internal calls should pass variants such as `ProbeStarted`, `OperatorPromptQueued`, and `PiAbortFailed`, not strings. Pi RPC record strings should be parsed once with `pi_event.from_string(record.type_)` before constructing `PiUpdate`. Where runner logic branches on `record.type_` for `"agent_end"` or `"extension_ui_request"`, bind `let event = pi_event.from_string(record.type_)` and pattern match on `AgentEnd` or `ExtensionUiRequest` so the state-machine branch does not depend on raw literals. Do not change pi RPC wire encoders in `src/scherzo/agent/pi_rpc.gleam` in this milestone.

25. Update `src/scherzo/orchestrator/event_publisher.gleam` event classification functions `kind_for_update`, `status_for_update`, and `pi_type_for_update` to pattern match on `update.event`. Unknown events with raw JSON should become `session_event.PiRaw`; known events should keep the intended `EventKind` and status behavior. Runner lifecycle events that have no raw JSON, such as `OperatorPromptQueued`, should keep the current lifecycle classification behavior.

26. Update any tests that assert `update.event == "..."` to assert variants and separately assert `pi_event.to_string(update.event) == "..."` where boundary output still uses a string. Add or update `test/orchestrator_event_publisher_test.gleam` cases so `MessageUpdate` is still not logged noisily, `ExtensionUiRequest` with blocking methods still maps to `UiRequest` and `WaitingUi`, `UnknownPiEvent("future")` with raw JSON maps to `PiRaw`, and `OperatorPromptQueued` remains a lifecycle event.

27. Run:

    grep -R "event: String" -n src/scherzo/agent/runner.gleam
    grep -R "lifecycle_update(\|lifecycle_update_with_message(\|lifecycle_update_with_request(\|token_update(" -n src/scherzo/agent/runner.gleam
    grep -R "update.event" -n src/scherzo --include='*.gleam'
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    The first grep should return no matches. Inspect the second grep and ensure runner update constructors pass variants, not string literals. Inspect the third grep and ensure remaining branches pattern match on variants or call `pi_event.to_string` only at boundaries such as logs, JSON, or protocol output. Record the milestone.

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
    }

    pub type EventName {
      LifecycleName(LifecycleEventName)
      PiName(pi_event.PiEvent)
    }

    Add `name_to_string` and `name_from_string`. `PiName` means "a worker update name carried by `runner.PiUpdate`" and may include runner lifecycle variants such as `OperatorPromptQueued`, not only raw pi RPC records. `name_from_string` may return `Result(EventName, Nil)` and reject unknown non-pi names rather than preserving them.

29. Change `EventPayload.name` from `String` to `EventName`. Change `empty_payload` to accept `EventName`. Change `session/json.gleam` and `control/protocol.gleam` to convert through `name_to_string` and `name_from_string`.

30. Update `event_publisher.lifecycle` and any daemon helper or direct `hub.publish` call that emits lifecycle names to accept `LifecycleEventName`. Replace direct string names such as `"dispatch_started"`, `"worker_started"`, `"stop_requested"`, `"worker_exited"`, `"worker_down"`, `"retry_scheduled"`, `"operator_command"`, and `"step_started"` with variants. When converting a `runner.PiUpdate`, use `PiName(update.event)`.

31. Run:

    grep -R "name: String" -n src/scherzo/session/event.gleam
    grep -R "event_publisher.lifecycle(.*\"" -n src/scherzo/orchestrator --include='*.gleam' || true
    grep -R "name: \"" -n src/scherzo/orchestrator --include='*.gleam' || true
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    The first grep should return no matches. The second and third greps are heuristics; inspect any hits and remove lifecycle name string literals from internal publishing while keeping boundary conversion tests intentional. Record the milestone.

32. Create `src/scherzo/tracker/state.gleam` and `src/scherzo/tracker/kind.gleam`, unless the current tree has already created owner modules where these types should live. In `tracker/kind.gleam`, define `TrackerKind { LinearTracker }` with `to_string` and `from_string`. In `tracker/state.gleam`, define an issue state wrapper and a typed normalized key:

    pub type IssueState {
      IssueState(raw: String, key: IssueStateKey)
    }

    pub type IssueStateKey {
      IssueStateKey(normalized: String)
    }

    pub fn from_string(value: String) -> Result(IssueState, Nil)
    pub fn to_string(state: IssueState) -> String
    pub fn key(state: IssueState) -> IssueStateKey
    pub fn key_from_string(value: String) -> IssueStateKey
    pub fn key_to_string(key: IssueStateKey) -> String
    pub fn equals_key(state: IssueState, expected: IssueStateKey) -> Bool
    pub fn equals_normalized(left: IssueState, right: IssueState) -> Bool
    pub fn todo() -> IssueState
    pub fn todo_key() -> IssueStateKey

    `key_from_string` must perform the current normalization behavior, `string.trim` followed by `string.lowercase`. Do not add a new non-empty validation rule in this plan. Make `from_string` return `IssueState(raw: string.trim(value), key: key_from_string(value))` for any string and keep existing `issue_has_required_fields` checks responsible for rejecting incomplete issues.

33. Change `TrackerConfig.kind` from `String` to `TrackerKind`. Update `config.default_tracker_config` and `resolve_tracker` so they parse `"linear"` once and store `LinearTracker`. Keep `error.UnsupportedTrackerKind(other)` for unknown strings, where `other` is the same lowercased value currently returned by `resolve_tracker` for unsupported kinds.

34. Change `Issue.state` and `BlockerRef.state` from `String` and `Option(String)` to `IssueState` and `Option(IssueState)`. Change `TrackerConfig.active_states` and `terminal_states` to `List(IssueState)`. Change `AgentConfig.max_concurrent_agents_by_state` to `Dict(IssueStateKey, Int)`. Update Linear decoders, test issue constructors, config active/terminal state parsing, config per-state limit parsing, `src/scherzo/agent/runner.gleam` final-state classification, `src/scherzo/orchestrator/core.gleam` active/terminal/blocker checks, `src/scherzo/orchestrator/daemon.gleam` per-state dispatch slot checks, `src/scherzo/linear.gleam`, `src/scherzo/smoke.gleam`, and `src/scherzo/linear_contract.gleam` boundary calls to use `tracker/state` helpers and convert to strings only when calling Linear or producing human-facing text.

35. Replace `normalize(issue.state) == "todo"` in `src/scherzo/orchestrator/core.gleam` with a named typed policy. The behavior-preserving change for this plan is `issue_state.equals_key(issue.state, issue_state.todo_key())`. Replace per-state dispatch-limit comparisons such as `normalize_state(issue.state) == normalized_state` with comparisons of `IssueStateKey` values. Do not add a configurable blocker policy or change workflow file syntax in this plan.

36. Add tests in `test/tracker_state_test.gleam` or `test/domain_test.gleam`: parsing `"Todo"` and `" todo "` should produce equal `IssueStateKey` values but preserve the trimmed display string chosen by the implementation; `IssueStateKey` lookup should make `max_concurrent_agents_by_state` with YAML key `todo` match an issue state `"Todo"`; `TrackerKind.from_string("linear")` and `TrackerKind.from_string("LINEAR")` should return `LinearTracker`; unknown tracker kind should produce the same config error as before.

37. Run:

    grep -R "kind: String" -n src/scherzo/domain.gleam src/scherzo/config 2>/dev/null || true
    grep -R "state: String" -n src/scherzo/domain.gleam src/scherzo/tracker 2>/dev/null || true
    grep -R "max_concurrent_agents_by_state: Dict(String" -n src test --include='*.gleam' || true
    grep -R "normalize_state" -n src/scherzo/orchestrator --include='*.gleam' || true
    grep -R "== \"todo\"" -n src test --include='*.gleam'
    direnv exec . gleam format --check src test
    direnv exec . gleam test

    The `kind: String`, `state: String`, typed-map, `normalize_state`, and `== "todo"` searches should return no internal source matches. Record the milestone.

38. Perform a final cleanup pass. Search for internal comparisons against the typed strings this plan replaced:

    grep -R "artifact.status ==" -n src test --include='*.gleam'
    grep -R "\"worker_down\"\|\"operator_abort\"\|\"operator_stop_after_current_turn\"\|\"stopped\"" -n src/scherzo/orchestrator src/scherzo/session --include='*.gleam'
    grep -R "\"probe_started\"\|\"turn_finished\"\|\"extension_ui_request\"\|\"operator_prompt_queued\"" -n src/scherzo/orchestrator src/scherzo/agent --include='*.gleam'
    grep -R "due_at_ms" -n src test --include='*.gleam'

    Remaining strings should be in conversion functions, tests that assert conversion, external decoders, external encoders, or logs that intentionally name log events. Move any remaining internal branch to typed pattern matching.

39. Run final validation:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

    Update this plan's Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective. Record the final change with `jj describe -m "Type internal state machine values"`.

## Testing and Falsifiability

This plan is falsified if scheduler behavior still depends on raw status strings. The concrete test is `grep -R "artifact.status == \"success\"" -n src test --include='*.gleam'`, which must return no matches. It is also falsified if `runner.PiUpdate.event` remains a string, `EventPayload.name` remains a string, tracker kind remains a string in source types, per-state dispatch limits remain keyed by raw normalized strings, or any `due_at_ms` field remains in source or tests after the retry milestone.

Each new ADT must have conversion tests:

- `StepStatus`: `StepSucceeded` converts to `"success"`, `StepFailed` converts to `"failure"`, successful command artifacts are internally `StepSucceeded`, failing or timed-out command artifacts are internally `StepFailed`, and template locals still expose strings.
- `RetryReason`: each variant converts to the exact current log/event string.
- `ParkReason`: `ParkMaxRetryAttempts`, `ParkMaxSessionsPerIssue`, and `ParkOperator("manual")` convert to the chosen boundary strings.
- `WorkerExitReason`: known variants convert to current strings including `"operator_stop_after_current_turn"`, and unknown decoded strings are rejected.
- `PiEvent`: each known pi RPC and runner lifecycle event converts to the current event string, unknown pi RPC events are represented as `UnknownPiEvent(raw)`, `event_publisher` event kind/status/pi-type classification matches current test expectations, and daemon logging uses `pi_event.to_string` rather than raw event fields.
- `EventName`: lifecycle variants convert to current EventHub names, pi names delegate to `PiEvent`, and unknown decoded non-pi names are rejected.
- `TrackerKind`: `"linear"` and `"LINEAR"` parse to `LinearTracker`; unknown values still produce `UnsupportedTrackerKind`.
- `IssueState`: normalization behavior matches current `string.trim |> string.lowercase` behavior used by `orchestrator/core.gleam` and `orchestrator/daemon.gleam`, the blocker policy still treats `Todo` as the state where blockers must be terminal, and `AgentConfig.max_concurrent_agents_by_state` matches issue states through `IssueStateKey` values rather than raw strings.

Existing tests that must remain green include `test/step_artifact_test.gleam`, `test/command_step_test.gleam`, `test/workflow_scheduler_test.gleam`, `test/workflow_run_test.gleam`, `test/orchestrator_core_test.gleam`, `test/orchestrator_daemon_test.gleam`, `test/orchestrator_daemon_session_event_test.gleam`, `test/orchestrator_event_publisher_test.gleam`, `test/session_event_test.gleam`, `test/control_protocol_test.gleam`, `test/agent_runner_test.gleam`, `test/pi_rpc_test.gleam`, `test/linear_test.gleam`, `test/smoke_test.gleam`, `test/linear_contract_test.gleam`, `test/config_test.gleam`, `test/state_record_test.gleam`, `test/state_projection_test.gleam`, `test/state_ledger_test.gleam`, and `test/state_compaction_test.gleam`.

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
    ! grep -R "max_concurrent_agents_by_state: Dict(String" -n src/scherzo/domain.gleam

If the current tree has already moved those types, run the equivalent checks against the files that now own config, issue, and agent policy types.

Behavior acceptance requires typed behavior to be observable at current boundaries, not backward-compatible with old payloads. EventHub JSON, control protocol JSON, workflow template locals, durable state ledger/projection JSON, logs for retry/park/worker exit reasons, and pi event names should reflect the new clean typed field names and conversions. Unknown pi RPC event names should still be represented as `UnknownPiEvent(raw)` because pi RPC is external input; unknown decoded session exit reasons may be rejected.

## Rollout, Recovery, and Idempotence

This is an internal representation refactor with an intentional local durable-state schema break. It has no data migration and no operator-facing feature flag. Roll it out as normal code once tests and structural checks pass, then delete or regenerate old `.scherzo-state/ledger` data before relying on the local durable ledger again.

Keep each milestone green and separately described with Jujutsu. If a later broad milestone such as issue state typing causes regressions, revert that milestone while keeping earlier localized improvements like `StepStatus`. Do not leave half-migrated records where both raw strings and typed variants represent the same internal concept.

The steps are safe to repeat. Running grep checks, format, and tests multiple times is safe. Old local durable state is disposable for this plan; do not add compatibility aliases solely to read it.

## Artifacts and Notes

Current examples verified during plan authoring:

    src/scherzo/domain.gleam: Issue.state is String.
    src/scherzo/domain.gleam: TrackerConfig.kind is String.
    src/scherzo/domain.gleam: ParkedEntry.reason is String.
    src/scherzo/domain.gleam: RetryEntry.due_at_ms is populated from delay_ms.
    src/scherzo/state/record.gleam: RetryScheduled encodes and decodes durable JSON field due_at_ms.
    src/scherzo/state/projection.gleam: RetryScheduled projection snapshots encode and decode durable JSON field due_at_ms.
    src/scherzo/session/event.gleam: EventPayload.name is String and Exited carries String.
    src/scherzo/agent/runner.gleam: PiUpdate.event is String and runner emits lifecycle strings such as operator_prompt_queued.
    src/scherzo/orchestrator/event_publisher.gleam: kind_for_update/status_for_update/pi_type_for_update branch on update.event strings.
    src/scherzo/step_artifact.gleam: StepArtifact.status is String.
    src/scherzo/workflow_scheduler.gleam: mark_finished branches on artifact.status == "success".
    src/scherzo/workflow_run.gleam: is_fatal_result branches on artifact.status == "success".
    src/scherzo/orchestrator/daemon.gleam: YAML command step session finish reason branches on artifact.status == "success".
    src/scherzo/orchestrator/core.gleam: blockers_satisfied checks normalize(issue.state) == "todo".
    src/scherzo/orchestrator/core.gleam and src/scherzo/orchestrator/daemon.gleam: per-state dispatch limits use normalized issue-state strings.

## Interfaces and Dependencies

The exact owner modules may shift if the current tree has already moved domain types, but the intended public shapes are:

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
      StopTerminal
      StopNonActive
    }

In `src/scherzo/session/reason.gleam`:

    pub type WorkerExitReason {
      Normal
      Failed
      OperatorAbort
      OperatorStopAfterCurrentTurn
      WorkerDown
      Stopped
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
      OperatorPromptSent
      OperatorPromptQueued
      OperatorPromptDropped
      OperatorUiTimeout
      PiAbortSent
      PiAbortFailed
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
    }

    pub type EventName {
      LifecycleName(LifecycleEventName)
      PiName(pi_event.PiEvent)
    }

In `src/scherzo/tracker/kind.gleam`:

    pub type TrackerKind {
      LinearTracker
    }

In `src/scherzo/tracker/state.gleam`:

    pub type IssueState {
      IssueState(raw: String, key: IssueStateKey)
    }

    pub type IssueStateKey {
      IssueStateKey(normalized: String)
    }

    pub fn from_string(value: String) -> Result(IssueState, Nil)
    pub fn to_string(state: IssueState) -> String
    pub fn key(state: IssueState) -> IssueStateKey
    pub fn key_from_string(value: String) -> IssueStateKey
    pub fn key_to_string(key: IssueStateKey) -> String
    pub fn todo() -> IssueState
    pub fn todo_key() -> IssueStateKey
    pub fn equals_key(state: IssueState, expected: IssueStateKey) -> Bool
    pub fn equals_normalized(left: IssueState, right: IssueState) -> Bool

After Milestone 6, whichever module owns `AgentConfig` must use the typed key for per-state dispatch limits:

    max_concurrent_agents_by_state: Dict(issue_state.IssueStateKey, Int)
