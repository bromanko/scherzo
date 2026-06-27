# Make retry-step acknowledge as a durable queued control operation

This ExecPlan review is a concise human-facing companion to the structured implementation pack. The implementation pack carries the mechanical file-by-file steps.

## Purpose / Big Picture

Operators and UI callers should be able to submit a slow `retry-step` repair and promptly learn that the daemon accepted durable work, rather than waiting for tracker refresh, artifact recovery, ledger repair, and scheduling to finish behind the request/response protocol. After this change, a successful `retry-step` acknowledgement is `queued` with an operation id, and later ledger/query/session evidence shows whether the repair completed, failed validation, or spawned resumed workflow work.

## Problem Framing and Constraints

Today the local control server waits up to `control.command_timeout` for mutating command results, while `retry-step` may do slow tracker checks, retained-artifact validation, workspace recovery, ledger writes, and recovered-worker spawning. A timeout at this layer is ambiguous because the daemon may still accept and execute the command after the caller sees `command_timeout`. The fix must not raise the timeout, must preserve explicit `rejected` and `not_found` cases for invalid requests, must remain compatible with the existing command-result protocol, and must avoid changing genuinely cheap controls such as pause/resume merely for architectural symmetry. The work must include test evidence, operator documentation and `scherzoctl` help updates before publish, and full validation with formatting, tests, and production linters. It must not change provider-live cleanup, remote-provider cache behavior, browser UI behavior, workflow YAML/schema behavior, or remote-provider semantics.

## Strategy Overview

Make `retry-step` an explicit asynchronous control operation. The daemon will perform only fast request validation and durable operation-intent recording before replying. The reply will be a normal `CommandResult` with status `queued`, a durable operation id, and target metadata. The durable lifecycle should record intent, running, completed, and failed states in ledger-backed records projected into a queryable read model. The existing retry-step repair path will then run from a queued daemon message and will append completion/failure status plus the existing workflow repair/session records so callers can correlate the acknowledgement with later observable work. Additive JSON fields and a small operation-status query keep sidecar/UI callers compatible while giving them a polling path.

## Alternatives Considered

Increasing `control.command_timeout` was rejected because it hides the ambiguity and still fails for sufficiently slow backends. Running the whole repair in a detached process without a durable intent was rejected because an acknowledged command could disappear across crashes and would not be queryable. Replacing the control protocol with a broader job system was rejected as disproportionate; a small durable operation layer around the existing command path solves the observed `retry-step` failure with less risk.

## Risks and Countermeasures

A queued operation could be executed twice after retries or restart; the countermeasure is durable operation ids, completion records, startup replay that skips completed operations, and tests proving duplicate queued retry-step work does not append duplicate `workflow_repair_requested` or `step_attempt_superseded` records. A queued acknowledgement could claim acceptance before intent is durable; the countermeasure is to send `queued` only after the operation-intent ledger append succeeds, otherwise return `rejected` with `ledger_append_failed`. A protocol change could break clients; the countermeasure is an optional `operation_id` field in existing command-result JSON and codec tests for older responses. A later repair failure could become invisible; the countermeasure is completion records, the operation-status query, existing ledger diagnostics, and existing session/event output. Operator docs or helper text could drift from the new asynchronous behavior; the countermeasure is to update `docs/runbooks/workflow-recovery.md`, the `retry-step` help in `src/scherzo/ctl/command_registry.gleam`, and any existing `retry-step` getting-started text that would otherwise remain misleading. The work could accidentally broaden into unrelated provider-live/cache or browser behavior; the countermeasure is to keep those files and behaviors out of scope and require acceptance evidence that no provider-live cleanup, remote-provider cache, or browser UI migration was introduced.

## Scope Boundaries

In scope is the `retry-step` local/sidecar control flow, command-result metadata, durable operation records, operation-status query support, startup replay for queued retry-step operations, tests, and documentation that `retry-step` is queued. The required documentation/helper surfaces are `docs/runbooks/workflow-recovery.md`, `src/scherzo/ctl/command_registry.gleam` help text and help tests, and `docs/GETTING_STARTED.md` only where its existing `retry-step` text would otherwise be inaccurate. Out of scope are timeout increases, browser UI changes, provider-live cleanup changes, remote-provider cache behavior changes, workflow YAML/schema changes, and a wholesale job framework. Cheap synchronous controls remain synchronous. Other expensive controls may adopt the same operation layer later, but this plan treats `retry-step` as the required acceptance path.

## Milestones

Milestone 1 preserves and extends protocol compatibility. `src/scherzo/control/command.gleam` gains optional operation metadata on `CommandResult`, and codec/remote/UI bridge tests prove old result JSON without `operation_id` still decodes while new queued result JSON round-trips with the operation id. The proof is targeted coverage in existing command-result, remote-envelope, and remote UI websocket tests before any daemon behavior changes.

Milestone 2 adds durable operation recording and query projection. The implementation adds ledger record bodies for operation intent, running, completed, and failed states, folds them into `src/scherzo/state/projection.gleam`, exposes an `operation_status` query through `src/scherzo/control/query/types.gleam`, `src/scherzo/control/query/codec.gleam`, and `src/scherzo/orchestrator/query_runtime.gleam`, and proves that the operation id returned to the caller can be polled with the same status and target metadata.

Milestone 3 routes `retry-step` through queued acknowledgement and asynchronous completion while preserving existing fail-closed validation and repair records. The proof is a deterministic slow-backend or barrier test in `test/orchestrator_daemon_retry_step_test.gleam` that receives `queued` with an `operation_id` before the slow path is released, plus negative tests for missing targets, parked/non-active targets, validation failures, and operation-intent append failure returning `not_found` or `rejected` without queued async work or repair ledger records.

Milestone 4 hardens replay/idempotence and updates operator documentation so `command_timeout` once again means acknowledgement failure, not slow repair completion. The proof is startup replay evidence for incomplete queued operations, skip evidence for completed operations, duplicate-replay evidence with no duplicate repair records, `scherzoctl retry-step --help` output that explains queued operation ids and polling, runbook/getting-started updates where needed, and pre-publish local or fake-daemon transcript evidence for queued acknowledgement plus operation-status completion or failure.

## Progress

- [x] (2026-06-27 09:30Z) Reviewed the repository guidance and current control, daemon, retry-step, ledger, query, UI bridge, and documentation surfaces relevant to this plan.
- [x] (2026-06-27 09:45Z) Authored this review document and prepared the structured implementation pack for handoff.
- [x] (2026-06-27 10:10Z) Incorporated plan-review feedback by tightening milestone proof, negative-path acceptance, lifecycle/replay evidence, documentation/help evidence, and pre-publish manual evidence.
- [x] (2026-06-27 10:35Z) Mirrored the follow-up review feedback into the review document and implementation-pack intent: exact documentation/helper surfaces, provider-live/cache non-scope, full validation, and the distinction between pre-publish local evidence and deferred live dogfood evidence.

## Surprises & Discoveries

The current `CommandStatus` type already contains `Queued`, but `CommandResult` has no operation id and the retry-step daemon path still returns `applied` only after running the full repair. The daemon transition shell already has specialized operator-reply machinery, but shell commands such as retry-step run their expensive work before a result can be returned.

The repository already has `retry-step` operator text in `docs/runbooks/workflow-recovery.md`, `docs/GETTING_STARTED.md`, and `src/scherzo/ctl/command_registry.gleam`, so documentation acceptance must check both prose docs and helper output rather than treating docs as a generic afterthought.

## Decision Log

- Decision: Implement `retry-step` as the first explicit asynchronous control operation instead of changing every mutating command at once. Rationale: `retry-step` is the reported production failure and exercises the durable-intent, slow-backend, repair, and scheduling requirements without an oversized command-system rewrite. Date: 2026-06-27.
- Decision: Use additive command-result metadata and a small query instead of a protocol version bump. Rationale: existing local CLI and UI bridge clients already decode command-result JSON and can ignore unknown fields until they opt into polling. Date: 2026-06-27.
- Decision: Treat local or fake-daemon queued-acknowledgement evidence as a pre-publish requirement, and treat live dogfood against a real retained run as deferred operator evidence when no safe retained run exists. Rationale: the feature must be proven before handoff without forcing an unsafe retry against production-like retained work. Date: 2026-06-27.
- Decision: Keep provider-live cleanup, remote-provider cache behavior, browser UI behavior, and workflow YAML/schema behavior out of scope. Rationale: the production failure is the `retry-step` acknowledgement lifecycle; broadening into unrelated migrations would add risk without improving acknowledgement correctness. Date: 2026-06-27.

## Outcomes & Retrospective

Not yet implemented. The expected outcome is that `retry-step` callers observe `queued` quickly, can poll the operation id, and can follow later ledger/session evidence without mistaking slow repair work for a protocol timeout.

## Validation and Acceptance

Implementation is acceptable only with concrete evidence. Run `direnv exec . gleam format --check src test` and expect no formatting diff, run `direnv exec . gleam test` and expect the full deterministic suite to pass, then run `direnv exec . gleam run -m glinter` and `direnv exec . gleam run -m scherzo_lint` and expect no production lint errors. The test suite must include new assertions proving a slow retry-step backend returns `queued` before the slow path is released, an invalid or missing retry-step target returns `not_found` or `rejected` without queuing, parked or non-active targets still fail closed, a simulated operation-intent ledger append failure returns `rejected` with reason `ledger_append_failed`, no queued async work, and no repair ledger records, the operation id appears in command-result JSON and operation-status query output, operation status moves from `queued` to `running` or completed on success, asynchronous repair failure is recorded and queryable, startup replay resumes incomplete queued operations, completed operations are not replayed, and duplicate or replayed queued work does not duplicate `workflow_repair_requested` or `step_attempt_superseded` records. Documentation validation must include updated `docs/runbooks/workflow-recovery.md`, `src/scherzo/ctl/command_registry.gleam` helper text and tests for `scripts/scherzoctl retry-step --help`, and `docs/GETTING_STARTED.md` if its existing `retry-step` paragraph would otherwise be stale. The helper output must say that `retry-step` queues work, returns an `operation_id`, and tells callers to poll the operation-status query or inspect ledger/session/events for completion. Before publish, collect a short local or fake-daemon transcript that includes both `scripts/scherzoctl retry-step run:<run-id> --step <step-id> --json` returning `"status":"queued"` with `"operation_id"` and the follow-up operation-status query output for that id showing completed or failed status. No browser UI check or provider-live/cache migration evidence is required for pre-publish acceptance because those surfaces are explicitly out of scope. Live dogfood evidence against a real retained run is useful but may be deferred to a human/operator after handoff if no safe retained run exists.

## Rollout, Recovery, and Idempotence

The rollout is additive: old clients continue to understand `status`, `command`, `target`, `reason`, and `message`, while new clients use `operation_id` and the operation-status query. If the operation-intent append fails, the command is rejected and no asynchronous work starts. If the daemon stops after queuing but before completion, startup replay resumes incomplete queued retry-step operations; completed operations are not replayed. The change introduces no provider-live cleanup migration, remote-provider cache migration, browser rollout, or workflow YAML/schema migration. If the change must be backed out, the queued operation records are harmless historical ledger entries, and the retry-step command can return to the existing synchronous path after disabling startup replay.

## Open Questions and Clarifications Needed

No open questions for the `retry-step` acceptance path. Future work can decide whether recollection, artifact-publication retry, or schedule-run-now should opt into the same durable operation layer.