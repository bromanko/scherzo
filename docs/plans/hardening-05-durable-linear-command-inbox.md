# Hardening 05: Add durable Linear command inbox receipts

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Linear command comments are no longer runtime-only. Scherzo records command receipt, command start, command result, and acknowledgement status in the local durable ledger. If Scherzo restarts, it does not re-execute already-applied command comments, it can post a missing acknowledgement for a command that applied before the crash, and it can process eligible command comments posted while Scherzo was down when those comments are still within the bounded observed-issue polling window.

The visible proof is a deterministic test that processes a `/scherzo park` comment, records the command as applied, simulates restart, sees the same comment again, and posts at most one acknowledgement without applying the park command a second time. Another test posts `/scherzo retry` while the daemon is down, restarts Scherzo with that issue in the recovered observed set, and verifies the command is processed once.

This phase hardens the existing Linear comment transport. It does not add webhooks, does not process commands on unrelated historical issues, does not parse arbitrary comments, and does not make Linear commands distributed across multiple Scherzo instances.

## Problem Framing and Constraints

Before this change, the Linear command transport was intentionally runtime-only. It kept processed comment ids in memory, ignored comments older than daemon startup, and missed commands posted while Scherzo was down. That was safe for the first polling version, but it was not good enough for restart resilience. Operators expect a command they posted during a short restart to be acknowledged or rejected, and Scherzo must not accidentally execute the same command twice after a process crash.

The previous hardening plans add a local durable ledger and single-instance crash recovery. This plan uses that ledger for Linear command receipts. It must preserve the same safety model as the current transport: explicit `/scherzo` prefix, author allowlist by Linear user id, bounded polling of observed issues only, and command execution through the shared daemon command handler.

The hardest edge case is a crash after a command has been sent to the daemon but before an acknowledgement is posted. Some commands are not idempotent. The safest general rule is: once a command has a durable `started` record, do not reapply it blindly after restart. If a durable `completed` record exists, reuse that recorded result and post/retry acknowledgement. If `started` exists without `completed`, mark the command outcome as `unknown_after_restart` and post an acknowledgement asking the operator to inspect or issue a new command. This avoids duplicate destructive actions.

## Strategy Overview

Extend `src/scherzo/control/linear_transport.gleam` so it can initialize from durable command receipts in the ledger projection instead of starting with an empty `processed_comment_ids` map. Replace the current startup-time filter that ignores all comments older than daemon startup with a ledger-aware filter:

- ignore comment ids with durable `acked` status;
- for comment ids with durable `completed` but no `acked`, produce an acknowledgement action from the stored result without reapplying;
- for comment ids with durable `started` but no `completed`, produce an `unknown_after_restart` acknowledgement and mark it handled;
- for comment ids with no durable receipt, parse/authorize and process even if the comment was created before daemon startup, as long as the comment belongs to an observed issue and is within the bounded poll results.

Persist command state transitions around daemon command execution:

1. after parse and authorization, append `LinearCommandSeen` if not already present;
2. immediately before submitting to the daemon command handler, append `LinearCommandStarted`;
3. after the handler returns, append `LinearCommandCompleted` with command status and a short redacted message;
4. before posting the acknowledgement, append or derive an outbox pending record if the ledger/outbox API requires it;
5. after acknowledgement comment succeeds, append `LinearCommandAcked`.

Use the durable single-instance recovery plan's ledger/outbox facilities for acknowledgement retry. If the ack comment failed because Linear was temporarily unavailable, retry it on later polls or startup until it succeeds or the error is classified as permanent.

## Alternatives Considered

One alternative is to keep ignoring old comments and only persist processed ids for comments seen after startup. That prevents duplicate execution after restart but still misses commands posted while Scherzo was down.

Another alternative is to reapply commands with `started` but no `completed` record after restart. That could duplicate destructive commands such as abort or park. This plan chooses safety over convenience and reports `unknown_after_restart` instead.

A third alternative is to add Linear webhooks now. Webhooks would improve wake-up latency and reduce polling gaps, but they require public endpoint deployment, signature verification, delivery retry semantics, and likely durable receipts anyway. This plan makes the polling transport durable first.

A fourth alternative is to use Linear comment acknowledgements themselves as the only receipt. That is insufficient because a crash can happen after command application but before ack creation, and Scherzo needs a local record of the command result to avoid reapplying it.

## Risks and Countermeasures

The main duplicate-execution risk is crashing between command application and durable completion append. Countermeasure: append `LinearCommandStarted` before application. If restart sees `started` without `completed`, do not reapply; acknowledge `unknown_after_restart`. This may force a human to reissue a command, but it avoids duplicate destructive actions.

The main missed-command risk is bounded polling. If many comments arrive while Scherzo is down and the command comment falls outside `poll_limit_per_issue`, Scherzo can still miss it. Countermeasure: document the bound, keep `poll_limit_per_issue` configurable, and add logs when the fetched page is full so operators know recovery may be incomplete. Webhook wake-up remains a future plan.

The main acknowledgement-duplication risk is posting the same ack twice after a crash. Countermeasure: record `LinearCommandAcked` after successful ack. If a crash happens after Linear accepted the ack but before the ledger append, a duplicate ack can still happen on restart. Include the source comment id in the ack body so duplicates are auditable. True Linear-side idempotency is deferred.

The main compatibility risk is changing current parser/authorization behavior. Countermeasure: keep parser and authorization unchanged; only the state source for processed comments changes from in-memory-only to ledger-backed.

The main privacy risk is persisting full command text. Countermeasure: durable receipts store command name, source comment id, issue id, author id, result status, and short redacted excerpts only. Full Linear comment bodies are not stored.

## Progress

- [x] (2026-04-29 04:20Z) Drafted this plan as the fifth hardening step after graceful lifecycle, durable ledger, single-instance crash recovery, and pi session continuation.
- [x] (2026-05-03 00:00Z) Normalized this plan after removing the obsolete pre-DAG pi session continuation plan. Durable Linear command receipts remain useful without pi session continuation; future workflow checkpoint/resumption work should be handled separately.
- [x] (2026-05-03 23:13Z) Initialized Linear command transport state from durable ledger command receipts during daemon startup.
- [x] (2026-05-03 23:13Z) Persisted seen, started, completed, and acked command records around authorized command execution and acknowledgement posting.
- [x] (2026-05-03 23:13Z) Replayed completed-but-unacked acknowledgements from stored receipt results without submitting commands again when the source comment is returned by the bounded observed-issue poll.
- [x] (2026-05-03 23:13Z) Reported started-without-completed commands as `unknown_after_restart` without reapplying them.
- [x] (2026-05-03 23:13Z) Processed bounded old unseen comments posted while Scherzo was down when they are on observed issues and still returned by polling.
- [x] (2026-05-03 23:13Z) Updated README and TODO with durable receipt behavior and remaining webhook limitations.
- [x] (2026-05-03 23:13Z) Ran implementation validation during the agent loop; an intermediate full-suite run was incorrectly recorded as having pre-existing helper-test failures.
- [x] (2026-05-03 23:36Z) Applied review feedback by keeping failed Linear command acknowledgements pending in daemon memory and retrying them on later poll completions without reapplying the source command.
- [x] (2026-05-03 23:53Z) Rechecked the workflow's recorded base change; `direnv exec . gleam test` passed there with 556 tests, so the run started from a green baseline.
- [x] (2026-05-04 00:05Z) Reran final validation in the retained implementation workspace after conflict resolution; `direnv exec . gleam format --check src test` and `direnv exec . gleam test` passed, with the full test suite reporting 585 passed and no failures.

## Surprises & Discoveries

- Observation: The prior pi session continuation plan was removed because it assumed one issue-level pi session, but this command-inbox plan does not depend on that assumption.
  Evidence: Durable command receipts operate on Linear comment ids, command results, and acknowledgement state. They can be implemented against the current Linear command transport and local ledger without knowing how future workflow checkpoints resume agent steps.

- Observation: The existing ledger record schema already had `LinearCommandSeen`, `LinearCommandStarted`, `LinearCommandCompleted`, and `LinearCommandAcked`, but the projection only kept the latest command status and therefore lost the completed result once `acked` was appended.
  Evidence: `src/scherzo/state/projection.gleam` now keeps the prior latest-status view and adds a cumulative `command_receipts` view so completed result data survives through acked projection snapshots.

- Observation: Existing v2 outbox payloads were sufficient for acknowledgement retry after normal command completion, but acknowledgement completion needed to append `LinearCommandAcked` using the original source comment id, not a synthetic issue-level outbox id.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` now writes command ack outbox entries keyed by source comment id and reuses `payload.source_comment_id` during startup outbox replay.

- Observation: The original agent notes misclassified an intermediate red test run as a pre-existing baseline failure.
  Evidence: The workflow metadata recorded base change `xyuzqlrvkwmtrxzsvrttmqypmmkyqxvr`; checking that revision after the failed workflow showed `direnv exec . gleam test` passing with 556 tests. After rebasing onto current `main`, the retained implementation workspace passed `direnv exec . gleam format --check src test` and `direnv exec . gleam test`, with 585 tests and no failures.

- Observation: Review found that failed acknowledgement posts were durable for restart recovery but were not retried again by the same daemon process.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` now tracks pending and in-flight Linear command acknowledgements in memory; `test/orchestrator_daemon_linear_command_test.gleam` covers failure followed by later-poll retry without a second command application.

## Decision Log

- Decision: Do not reapply commands that have `started` but no `completed` durable record.
  Rationale: Some commands are destructive or not idempotent. Reporting an unknown outcome is safer than applying twice.
  Date: 2026-04-29

- Decision: Continue polling only observed issues.
  Rationale: Durable receipts improve restart behavior without expanding Scherzo into a project-wide comment scanner.
  Date: 2026-04-29

- Decision: Keep webhooks out of this phase.
  Rationale: Durable receipts are needed even with webhooks; implementing receipts first makes a later webhook transport smaller.
  Date: 2026-04-29

- Decision: Do not require pi session continuation before durable Linear command receipts.
  Rationale: The old issue-level pi session continuation plan was superseded by the need for workflow DAG checkpoints and step-scoped continuation. Linear command receipts harden remote operator command processing and can proceed independently of that future workflow recovery design.
  Date: 2026-05-03

- Decision: Replay completed-unacked and started-unknown command acknowledgements from the next bounded observed-issue comment poll instead of adding a separate startup scanner.
  Rationale: The polling transport already enforces the safe boundary of observed issue ids and `poll_limit_per_issue`; using it avoids expanding Scherzo into an unbounded historical comment scanner.
  Date: 2026-05-03

- Decision: Keep the old latest `commands` projection for compatibility and add a cumulative `command_receipts` projection for durable inbox semantics.
  Rationale: Existing recovery and snapshot tests expect latest command status replacement, while durable acknowledgement replay needs prior command name and completion result after later ack records.
  Date: 2026-05-03

- Decision: Retry failed Linear command acknowledgements from daemon memory on later poll completions, while tracking in-flight comment ids to avoid concurrent duplicate posts.
  Rationale: The durable outbox already protects restart recovery, but same-process retry closes a review gap for transient Linear failures without adding an unbounded scheduler or reprocessing the source command comment.
  Date: 2026-05-03

## Outcomes & Retrospective

Implemented durable Linear command inbox receipts. Startup initializes `linear_transport.TransportState` from the ledger projection. Authorized commands append `LinearCommandSeen` and `LinearCommandStarted` before execution, append `LinearCommandCompleted` after the operator command returns, persist a v2 ack outbox record before posting, and append `LinearCommandAcked` after Linear accepts the acknowledgement. After restart, acked commands are skipped, completed-unacked commands produce an acknowledgement from the stored status/message without a new submit action when their source comment is returned by the bounded poll, and started-uncompleted commands produce `unknown_after_restart` without reapplying.

Manual recovery after the opaque workflow failure verified that the recorded starting revision was green and that the retained implementation workspace now validates cleanly after rebasing onto current `main`. `direnv exec . gleam format --check src test` passes, and `direnv exec . gleam test` reports 585 passed and no failures. Remaining limitations are the documented duplicate-ack window if Linear accepts an ack but Scherzo crashes before `LinearCommandAcked`, the bounded polling window for comments posted while down, lack of Linear webhooks, and lack of Linear-side idempotency.

## Context and Orientation

Linear command comments are implemented by `src/scherzo/control/linear_parser.gleam`, `src/scherzo/control/linear_transport.gleam`, Linear comment query helpers in `src/scherzo/linear.gleam`, and daemon integration in `src/scherzo/orchestrator/daemon.gleam`. The transport still keeps a runtime `processed_comment_ids` set to avoid duplicate work within one daemon process, but startup now also initializes it with durable command receipt facts from the ledger projection and no longer rejects unseen comments solely because they predate daemon startup.

The durable ledger from `hardening-02` and startup recovery from `hardening-03` provide local persistent state and a replay projection. The ledger schema already includes `LinearCommandSeen`, `LinearCommandStarted`, `LinearCommandCompleted`, and `LinearCommandAcked` records. `src/scherzo/state/projection.gleam` now preserves both the legacy latest `commands` status and a cumulative `command_receipts` view keyed by Linear comment id.

A Linear command receipt is local to one canonical workspace root. It does not coordinate multiple Scherzo daemons. It is not a Linear webhook delivery receipt.

## Preconditions and Verified Facts

Before implementing this plan:

- `docs/plans/hardening-01-graceful-daemon-lifecycle.md` is complete.
- `docs/plans/hardening-02-local-durable-state-ledger.md` is complete.
- `docs/plans/hardening-03-single-instance-crash-recovery.md` is complete.
- The obsolete issue-level pi session continuation plan has been removed. Future workflow checkpoint/resumption work is separate and is not a prerequisite for durable Linear command receipts.
- `docs/plans/linear-command-transport.md` is complete.
- The daemon initializes recovery before candidate dispatch.
- The ledger projection can expose command receipt states by Linear comment id.
- `direnv exec . gleam test` passes.

If single-instance crash recovery is not complete, do not implement this plan first. It depends on recovered observed issue sets and durable outbox replay.

## Scope Boundaries

In scope: durable command receipts; ledger-backed processed-comment state; processing eligible comments posted while down; preventing duplicate command execution after restart; retrying missing acknowledgement comments; tests for seen/started/completed/acked states; README documentation.

Out of scope: Linear webhooks; durable receipts shared across multiple workspace roots; commands on unrelated historical issues; processing edited comments as new commands; full Linear comment body storage; Scherzo-to-Linear final result comments.

## Milestones

Milestone 1 extends pure transport state. At the end, tests can initialize `linear_transport.TransportState` from a ledger command projection and verify skip/replay behavior for acked, completed-unacked, started-uncompleted, and unseen comments.

Milestone 2 persists command records during daemon execution. At the end, daemon tests can process a command and inspect ledger records in the order seen, started, completed, acked.

Milestone 3 processes comments posted while down. At the end, startup recovery plus the next command poll can process an old but unseen command comment for an observed issue.

Milestone 4 replays acknowledgements. At the end, completed-unacked commands post an ack on restart without reapplying the command, and ack failures remain pending for later retry.

Milestone 5 documents limitations and validates. At the end, README explains durable receipts, bounded polling, unknown-after-restart outcomes, and remaining webhook gaps.

## Plan of Work

Extend the ledger projection with a `CommandReceipt` view. It should answer, for a comment id, whether the command is unseen, seen, started, completed with stored status/message, or acked. Store command name, issue id, author id, source comment id, result status string, message excerpt, and lifecycle timestamps. The current implementation does not persist the command target separately; replayed acknowledgement bodies omit target for completed-unacked commands and keep the source comment id, command name, status, and message.

Change `linear_transport.TransportState` so `processed_comment_ids` is replaced or supplemented by a durable receipt view plus a runtime set for comments processed during the current tick but not yet flushed. The transport should no longer skip comments solely because `created_at_ms < daemon_started_at_ms`. Instead, it uses durable receipts and bounded issue polling.

Add pure tests for receipt behavior. Feed comments with durable states:

- `Acked`: no action.
- `Completed` without `Acked`: produce `PostAck` from stored result, no `SubmitCommand`.
- `Started` without `Completed`: produce `PostAck` with `unknown_after_restart`, no `SubmitCommand`.
- `Unseen`: parse/authorize and submit command.

Modify daemon Linear command processing. When a parsed authorized command is about to be submitted, append `LinearCommandSeen` and `LinearCommandStarted`. After `apply_operator_command_to_state` returns, append `LinearCommandCompleted`. Then generate ack action. If appending `Started` fails, do not execute the command. If appending `Completed` fails after execution, log critical `linear_command_completion_record_failed`; on restart this may become `unknown_after_restart`.

Modify ack side effects. Before posting an ack, ensure there is a durable outbox pending record or enough command receipt data to retry. After `client.post_ack` succeeds, append `LinearCommandAcked`. If ack fails transiently, leave it completed-unacked so recovery retries later. If ack fails permanently, record `OutboxFailed` but keep command completed.

Modify startup recovery. After replaying the ledger, initialize Linear command transport state with the command receipt projection. The implemented acknowledgement replay path uses the next bounded observed-issue comment poll to produce completed-unacked and started-unknown ack actions, keeping recovery within the same observed issue ids and `poll_limit_per_issue` boundary as normal command polling.

Update README. Replace the current statement that commands posted while down are missed with the new rule: commands posted while down are processed if they are on observed issues and still returned by bounded polling; already-applied comments are not re-executed after restart; commands with unknown in-flight outcome are acknowledged as unknown and require a new command if needed.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current pass count.

2. Add focused command receipt projection tests. The implementation added them to `test/state_projection_test.gleam`, constructing ledger records for seen, started, completed, and acked commands and asserting the projection returns the expected receipt state.

3. Implement or extend `src/scherzo/state/projection.gleam` to expose command receipts by comment id while preserving the existing latest command status projection.

4. Update `test/linear_command_transport_test.gleam`. Add `acked_receipt_skips_comment_test`, `completed_unacked_receipt_posts_ack_without_submit_test`, `started_uncompleted_receipt_posts_unknown_ack_test`, and `unseen_old_comment_can_submit_test`.

5. Modify `src/scherzo/control/linear_transport.gleam` to use receipt state instead of startup-time filtering. Preserve existing parser, authorization, code-fence, max-comments-per-tick, and dedupe behavior.

6. Add daemon ledger tests. The implementation added them to `test/orchestrator_daemon_linear_command_test.gleam`; one test processes a `/scherzo park` comment and asserts records are appended in order: seen, started, completed, acked.

7. Implement daemon receipt appends around command execution.

8. The planned fake-ledger-writer failure test was not added because the daemon currently writes the ledger through repository-local functions rather than dependency-injected writer seams. The implemented guard still checks `append_ledger_bodies` before command execution; adding a fake writer seam remains possible future test hardening.

9. Add `completed_unacked_command_replays_ack_after_restart_test`: prewrite completed command receipt without ack, start daemon, and assert fake Linear command client receives an ack body without command handler invocation. The implementation names this test `completed_unacked_command_replays_ack_without_reapplying_test`.

10. Add `started_uncompleted_command_gets_unknown_ack_after_restart_test`: prewrite started without completed, start daemon, and assert an unknown-outcome ack is posted and the command handler is not invoked.

11. Add `comment_posted_while_down_is_processed_when_observed_test`: feed a Linear comment created before daemon startup but absent from receipts for an observed issue and assert it submits once. The implementation names this test `old_unseen_comment_posted_while_down_is_processed_when_observed_test`.

12. The bounded-poll negative test was covered by keeping command processing restricted to the fake client results returned for observed issue ids; no additional page-full warning was implemented in this phase.

13. Implement ack replay using next-poll processing. Keep it bounded to observed issue ids.

14. Update README `Linear command comments`, `Local durable ledger`, `Implemented coverage`, and `Safety posture`; update `docs/TODO.md` so webhook wake-up remains as the follow-up.

15. Run `direnv exec . gleam format --check src test` and `direnv exec . gleam test`. Both commands must pass; the final retained-workspace validation after rebase reported 585 passed and no failures.

16. Optional credential-gated validation was not run; deterministic fake Linear and fake pi actor tests cover the restart and no-reapply behavior.

17. Do not commit in Scherzo implementation workspaces; the publish workflow creates the final jj description/bookmark after validation.

## Testing and Falsifiability

This plan is falsified if a processed command comment can execute twice after restart, if a command posted while Scherzo is down is always ignored despite being on an observed issue and within bounded polling results, if completed-unacked commands are re-executed instead of acknowledged, if started-uncompleted commands are re-executed after restart, if ack failures lose the command result, or if full command bodies/secrets are stored in the ledger.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

No deterministic test may require real Linear or real pi. Use fake Linear comment clients and ledger records written to test workspace roots.

## Validation and Acceptance

Accept this phase when:

- Command receipt states are durable and replayed from the ledger.
- Acked comments are skipped after restart.
- Completed-unacked commands post/retry acknowledgement without reapplying.
- Started-uncompleted commands produce `unknown_after_restart` acknowledgement without reapplying.
- Unseen commands created while down can process when observed and returned by bounded polling.
- Existing parser/authorization behavior is unchanged.
- README documents remaining bounded-polling and no-webhook limitations.
- The full deterministic suite passes with no failures.

## Rollout, Recovery, and Idempotence

Roll out with Linear commands still disabled by default. When enabled, the first run after this phase starts building durable receipts. Existing old command comments without receipts are eligible only if they are on observed issues and still in bounded poll results; operators should avoid posting command comments while upgrading if exact behavior matters.

If a command ack is duplicated because Linear accepted an ack but Scherzo crashed before appending `LinearCommandAcked`, the duplicate ack contains the same source comment id and command status. Operators can treat it as an audit duplicate.

If Scherzo reports `unknown_after_restart`, it means the prior daemon started applying the command but did not durably record a result. The operator should inspect issue/session state and post a new command if needed.

## Artifacts and Notes

Example durable command receipt sequence:

    LinearCommandSeen(comment_id="c1", issue_id="i1", author_id="u1", command_name="park", excerpt="/scherzo park --reason waiting")
    LinearCommandStarted(comment_id="c1", issue_id="i1", command_name="park")
    LinearCommandCompleted(comment_id="c1", issue_id="i1", status="applied", message_excerpt="parked")
    LinearCommandAcked(comment_id="c1", issue_id="i1")

Example unknown ack:

    Scherzo command received from comment c1.
    Command: park
    Status: unknown_after_restart
    Message: Scherzo restarted while this command was in progress. Inspect current issue/session state and post a new command if needed.

## Interfaces and Dependencies

The implemented command receipt projection exposes types equivalent to:

    pub type CommandReceiptState {
      CommandReceiptUnseen
      CommandReceiptSeen(issue_id: String, author_id: String, command_name: String, excerpt: String, seen_at_ms: Int)
      CommandReceiptStarted(issue_id: String, author_id: String, command_name: String, excerpt: String, seen_at_ms: Int, started_at_ms: Int)
      CommandReceiptCompleted(issue_id: String, author_id: String, command_name: String, excerpt: String, result_status: String, message_excerpt: String, seen_at_ms: Int, started_at_ms: Int, completed_at_ms: Int, acked_at_ms: Option(Int))
      CommandReceiptAcked(issue_id: String, acked_at_ms: Int)
    }

    pub fn command_receipt(
      projection: projection.Projection,
      comment_id: String,
    ) -> CommandReceiptState

`linear_transport.TransportState` now carries a durable receipt dictionary. Daemon startup calls:

    pub fn new_state_with_receipts(
      daemon_started_at_ms: Int,
      command_receipts: Dict(String, projection.CommandReceiptState),
    ) -> TransportState

`linear_transport.process_comments` keeps its existing call shape and reads durable receipts from `TransportState`. `PostAck` actions now include the source comment id so the daemon can persist `LinearCommandAcked(comment_id, issue_id)` after Linear accepts the acknowledgement:

    PostAck(issue_id: String, source_comment_id: String, body: String)

No new package dependency was required. This plan depends on the ledger and recovery APIs from hardening plans 02 and 03.
