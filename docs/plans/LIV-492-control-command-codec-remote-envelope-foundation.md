# Control command codec and remote envelope foundation

## Purpose / Big Picture

This plan creates the smallest safe foundation for future centralized UI/server control work: Scherzo will have one shared JSON codec for operator commands and command results, plus a remote envelope module that can describe control-plane messages without claiming any live remote daemon behavior. After implementation, a developer can run the Gleam test suite and see local control-file protocol round trips still pass, every existing operator command variant round-trip through the shared codec, and new remote envelope tests prove the intended message shapes and rejection behavior.

## Problem Framing and Constraints

A prior attempt tried to implement the full centralized control plane in one slice and did not pass the plan-completion gate. The useful salvage is at the helper/protocol boundary, not at the live integration layer. The current tree already has local control command types in `src/scherzo/control/command.gleam`, local control-file request/response framing in `src/scherzo/control/protocol.gleam`, loopback discovery fields in `src/scherzo/control/file.gleam`, and local tests in `test/control_command_test.gleam` and `test/control_protocol_test.gleam`.

The implementation must stay additive and small. It must not wire daemon startup, remote sockets, a server harness, command routing, daemon identity, or liveness tracking. Local control-file tokens and loopback discovery fields such as `token`, `host`, `port`, `workspace_root`, and control-file paths remain local-only concerns and must not appear in the remote envelope.

## Strategy Overview

Keep the domain model in `src/scherzo/control/command.gleam` and add JSON codec helpers there for `OperatorCommand` and `CommandResult`. The codec scope is all current command variants, not a representative subset: pause, resume, reload, retry issue, retry workflow step, park, unpark, abort, stop-after-current-turn, prompt, UI response, and schedule-run-now must all have explicit round-trip coverage, including the retry target forms and UI cancel/value forms that change payload shape. Then keep `src/scherzo/control/protocol.gleam` as the local adapter that still owns request ids, tokens, version checks, legacy aliases, and local error semantics. The local adapter should delegate only the command-result JSON shape, and any command-request refactor must be backed by existing round-trip tests so the local protocol remains compatible.

Add a new `src/scherzo/control/remote_envelope.gleam` module that defines only message shapes and pure encode/decode functions for `hello`, `heartbeat`, `server_command`, `command_receipt`, `command_result`, and a minimal state snapshot. Remote envelope decoding must reject bad envelope versions, unknown envelope types, missing required fields, malformed nested command payloads, mutually exclusive command fields, and unknown nested command types with stable error codes. This gives later work a tested format to build on while deliberately leaving transport, authentication, identity, harnessing, and routing out of scope.

## Alternatives Considered

One alternative is to leave command/result JSON embedded only in the local protocol module. That is lowest-change today, but it would force the remote envelope to duplicate the same command and result shapes immediately.

Another alternative is to port more of the failed centralized control-plane implementation, including startup wiring and server behavior. That repeats the monolithic risk that caused the prior failure and would blur whether this slice proves codecs or live behavior.

A third option is to create a separate codec module and refactor the whole local request decoder around it. That may be useful later, but it risks changing local alias and validation behavior. Keeping the codec near the command types and using compatibility tests is the smaller foundation.

## Risks and Countermeasures

The main risk is accidentally changing the existing local control-file protocol. The countermeasure is to keep local request framing in `src/scherzo/control/protocol.gleam` and require round-trip and error-path tests for existing local commands.

A second risk is silently under-testing the shared codec by covering only the easiest command payloads. The countermeasure is explicit codec round-trip coverage for every current `OperatorCommand` variant, including resume, reload, unpark, abort, stop-after-current-turn, every retry target form, both UI response forms, and schedule-run-now, plus all command result statuses and reason-preserving statuses.

A third risk is accepting malformed remote messages too loosely and forcing later transport work to reverse undocumented behavior. The countermeasure is negative test coverage for unknown command and envelope types, unsupported versions, missing required fields, mutually exclusive command fields, bad envelope shape, and invalid nested command payloads with stable error codes.

A fourth risk is leaking local loopback or token concepts into a future remote protocol. The countermeasure is explicit negative test coverage that encoded remote envelopes omit `token`, `host`, `port`, `workspace_root`, control-file path names, and `SCHERZO_CONTROL_FILE`, and that a local control-file JSON blob is not accepted as a remote envelope.

A fifth risk is overstating what works. The implementation must not add daemon startup wiring, socket clients, a server harness, liveness registry, or documentation/transcripts claiming a live UI/server control plane. The only manual pre-publish evidence for this foundation slice is a diff/scope audit proving those non-goals stayed out of the change.

A sixth risk is growing an already-large local protocol module. The implementation should avoid adding new internal imports to `src/scherzo/control/protocol.gleam`; if the source guardrail fails, shrink or split the new code rather than raising baselines for this slice.

## Scope Boundaries

In scope are pure JSON codecs for existing `OperatorCommand` and `CommandResult` values, compatibility-preserving local protocol refactoring around command-result JSON, a new pure remote envelope module, and focused tests for positive and negative message cases.

Out of scope are daemon identity, remote authentication, socket or HTTP transport, server lifecycle, startup wiring, command routing to a daemon, liveness or heartbeat registries, UI integration, new operator UX, schema publication, and product documentation claiming a live centralized control plane.

## Milestones

Milestone 1 establishes the shared codec boundary. At the end, `src/scherzo/control/command.gleam` can encode and decode command payloads and command results, and `test/control_command_test.gleam` proves every existing `OperatorCommand` variant round-trips through the codec. The test evidence must include pause, resume, reload, retry issue by id and identifier, retry workflow step by automatic target, issue reference, and run id with and without `step_id`, park, unpark, abort, stop-after-current-turn, prompt, UI cancel, UI value, and schedule-run-now. It must also prove all result statuses preserve their status strings and that `Rejected` and `NotAllowed` preserve reasons.

Milestone 2 preserves the local adapter while introducing shared result JSON. At the end, `src/scherzo/control/protocol.gleam` still owns local request ids, tokens, versions, aliases, and request errors, but delegates command-result JSON to the shared codec. `test/control_protocol_test.gleam` must still prove local mutating command request round trips, local alias decoding, invalid local request rejection, and command-result response preservation.

Milestone 3 adds the remote envelope foundation. At the end, `src/scherzo/control/remote_envelope.gleam` can encode and decode hello, heartbeat, server command, command receipt, command result, and minimal state snapshot envelopes without any local token or loopback fields. `test/control_remote_envelope_test.gleam` must prove every envelope shape round-trips, bad envelope version/type/shape is rejected, invalid nested command payloads are rejected with stable errors, and a local control-file JSON object is not accepted as a remote envelope.

Milestone 4 completes validation and scope audit. At the end, the required test, format, glinter, and Scherzo lint gates pass, and the final diff contains only codec/envelope/test changes with no live daemon/server wiring. The diff/scope audit is a pre-publish blocking manual check; browser checks, live-daemon dogfood checks, remote-server checks, and post-implementation operator checks are explicitly not required for this pure-codec foundation slice and are deferred to later tickets that add live behavior.

## Progress

- [x] (2026-05-22) Confirmed the prepared output target is `docs/plans/` and selected this review document path.
- [x] (2026-05-22) Reviewed the current command, local protocol, control-file, test, and source-guardrail files needed to frame a small implementation slice.
- [x] (2026-05-22) Authored this human-reviewable plan summary and separated mechanical implementation detail into the structured implementation pack.
- [x] (2026-05-22) Incorporated review feedback requiring full command-variant codec coverage, explicit invalid-payload evidence, and clear manual-check classification.

## Decision Log

- Decision: Keep the shared codec adjacent to `src/scherzo/control/command.gleam` rather than making the first slice a broad local protocol rewrite.
  Rationale: The command module owns the domain types, and the local protocol module already carries compatibility-sensitive ids, tokens, aliases, and error semantics.
  Date: 2026-05-22

- Decision: Model the remote work as pure envelope encode/decode only.
  Rationale: This creates a reusable foundation while avoiding the failed prior shape of attempting live daemon/server behavior in the same slice.
  Date: 2026-05-22

- Decision: Treat local control-file credentials and loopback discovery as explicit negative remote-envelope coverage.
  Rationale: Remote message shapes should not inherit local-only `token`, `host`, `port`, `workspace_root`, or control-file path concepts by accident.
  Date: 2026-05-22

- Decision: Require exhaustive coverage for all current operator command variants in the shared codec rather than representative sampling.
  Rationale: The codec is intended to be shared by future transports; partial coverage would let a later remote transport inherit untested payload behavior for less common commands.
  Date: 2026-05-22

- Decision: Classify only the diff/scope audit as manual pre-publish evidence for this slice.
  Rationale: The remote envelope is deliberately pure and unwired, so browser, dogfood, live-daemon, remote-server, and post-implementation operator checks would either be impossible or would encourage scope creep into live behavior.
  Date: 2026-05-22

## Validation and Acceptance

Acceptance requires evidence, not assertions. `direnv exec . gleam test` must pass and include tests proving local protocol command round trips still decode to the same `OperatorCommand` values, all current `OperatorCommand` variants round-trip through the shared command codec, all retry target forms and UI response forms survive encode/decode, malformed or invalid command payloads are rejected, every required remote envelope message type round-trips, bad remote envelope versions/types/shapes are rejected, invalid nested remote command payloads produce stable errors, result statuses and reasons survive encode/decode, and remote envelope JSON omits local token and loopback discovery fields. `direnv exec . gleam format --check src test`, `direnv exec . gleam run -m glinter`, and `direnv exec . gleam run -m scherzo_lint` must also pass.

The final implementation evidence must include `git diff --name-only` or equivalent review showing no daemon startup wiring, no remote transport/socket client, no server harness, no liveness registry, and no product docs or transcripts claiming a live UI/server control plane. This diff/scope audit is a required pre-publish manual check. No browser check, live remote-server check, dogfood check, or post-implementation operator check is required for this foundation slice; those checks are deferred until a later ticket wires the pure envelope into live behavior. If any gate fails for a pre-existing unrelated reason, the implementer must capture the exact command output and explain why the new codec/envelope changes are not the cause before handoff.

## Rollout, Recovery, and Idempotence

Rollout is effectively no-op at runtime because the new remote envelope is not wired into any daemon, server, or transport. Existing local control continues through `src/scherzo/control/protocol.gleam` and must remain covered by compatibility tests. The change is reversible by removing the new remote envelope module and tests and reverting the small codec delegation in the local protocol. Because there is no live remote path, rollout does not require a browser, dogfood, daemon, or remote-server exercise before publish; the required manual rollout evidence is limited to the pre-publish diff/scope audit described above.

The work is idempotent: running tests and format/lint commands repeatedly should not mutate tracked files beyond normal build artifacts. If implementation uncovers that a refactor would require source-guardrail baseline changes, broad protocol rewrites, or live server behavior, stop and reduce the slice rather than expanding the plan.

## Open Questions and Clarifications Needed

No blocking clarification is needed for this foundation slice. Future tickets should decide daemon identity, remote authentication, transport choice, server harness behavior, liveness semantics, and command routing once this codec and envelope foundation exists.
