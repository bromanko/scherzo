# Hardening 05: Add durable Linear command inbox receipts

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Linear command comments are no longer runtime-only. Scherzo records command receipt, command start, command result, and acknowledgement status in the local durable ledger. If Scherzo restarts, it does not re-execute already-applied command comments, it can post a missing acknowledgement for a command that applied before the crash, and it can process eligible command comments posted while Scherzo was down when those comments are still within the bounded observed-issue polling window.

The visible proof is a deterministic test that processes a `/scherzo park` comment, records the command as applied, simulates restart, sees the same comment again, and posts at most one acknowledgement without applying the park command a second time. Another test posts `/scherzo retry` while the daemon is down, restarts Scherzo with that issue in the recovered observed set, and verifies the command is processed once.

This phase hardens the existing Linear comment transport. It does not add webhooks, does not process commands on unrelated historical issues, does not parse arbitrary comments, and does not make Linear commands distributed across multiple Scherzo instances.

## Problem Framing and Constraints

The current Linear command transport is intentionally runtime-only. It keeps processed comment ids in memory, ignores comments older than daemon startup, and misses commands posted while Scherzo is down. That was safe for the first polling version, but it is not good enough for restart resilience. Operators expect a command they posted during a short restart to be acknowledged or rejected, and Scherzo must not accidentally execute the same command twice after a process crash.

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
- [ ] Initialize Linear command transport state from durable ledger receipts.
- [ ] Persist seen/started/completed/acked command records.
- [ ] Replay completed-but-unacked acknowledgements without reapplying commands.
- [ ] Report started-without-completed commands as `unknown_after_restart`.
- [ ] Process bounded commands posted while Scherzo was down for observed issues.
- [ ] Update README and TODO with remaining webhook limitations.

## Surprises & Discoveries

- Observation: The prior pi session continuation plan was removed because it assumed one issue-level pi session, but this command-inbox plan does not depend on that assumption.
  Evidence: Durable command receipts operate on Linear comment ids, command results, and acknowledgement state. They can be implemented against the current Linear command transport and local ledger without knowing how future workflow checkpoints resume agent steps.

During implementation, record whether existing ack outbox records were sufficient or needed command-specific fields.

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

## Outcomes & Retrospective

(To be filled at completion. Include final receipt states, final restart behavior, final test count, and any remaining duplicate ack windows.)

## Context and Orientation

Linear command comments are implemented by `src/scherzo/control/linear_parser.gleam`, `src/scherzo/control/linear_transport.gleam`, Linear comment query helpers in `src/scherzo/linear.gleam`, and daemon integration in `src/scherzo/orchestrator/daemon.gleam`. The transport currently keeps `processed_comment_ids` in memory and filters comments older than `daemon_started_at_ms`.

The durable ledger from `hardening-02` and startup recovery from `hardening-03` provide local persistent state and a replay projection. The ledger schema includes or can be extended to include `LinearCommandSeen`, `LinearCommandStarted`, `LinearCommandCompleted`, and `LinearCommandAcked` records.

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

Extend the ledger projection with a `CommandReceipt` view. It should answer, for a comment id, whether the command is unseen, seen, started, completed with a stored `CommandResult`, acked, or unknown/corrupt. Store command name, issue id, author id, source comment id, status string, target, and message excerpt.

Change `linear_transport.TransportState` so `processed_comment_ids` is replaced or supplemented by a durable receipt view plus a runtime set for comments processed during the current tick but not yet flushed. The transport should no longer skip comments solely because `created_at_ms < daemon_started_at_ms`. Instead, it uses durable receipts and bounded issue polling.

Add pure tests for receipt behavior. Feed comments with durable states:

- `Acked`: no action.
- `Completed` without `Acked`: produce `PostAck` from stored result, no `SubmitCommand`.
- `Started` without `Completed`: produce `PostAck` with `unknown_after_restart`, no `SubmitCommand`.
- `Unseen`: parse/authorize and submit command.

Modify daemon Linear command processing. When a parsed authorized command is about to be submitted, append `LinearCommandSeen` and `LinearCommandStarted`. After `apply_operator_command_to_state` returns, append `LinearCommandCompleted`. Then generate ack action. If appending `Started` fails, do not execute the command. If appending `Completed` fails after execution, log critical `linear_command_completion_record_failed`; on restart this may become `unknown_after_restart`.

Modify ack side effects. Before posting an ack, ensure there is a durable outbox pending record or enough command receipt data to retry. After `client.post_ack` succeeds, append `LinearCommandAcked`. If ack fails transiently, leave it completed-unacked so recovery retries later. If ack fails permanently, record `OutboxFailed` but keep command completed.

Modify startup recovery. After replaying the ledger, initialize Linear command transport state with the command receipt projection. Also enqueue ack replay for completed-unacked receipts whose issue ids are still in the observed durable set, or let the next poll produce those ack actions. Prefer explicit startup ack replay if it can be bounded and tested.

Update README. Replace the current statement that commands posted while down are missed with the new rule: commands posted while down are processed if they are on observed issues and still returned by bounded polling; already-applied comments are not re-executed after restart; commands with unknown in-flight outcome are acknowledged as unknown and require a new command if needed.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current pass count.

2. Add `test/linear_command_receipt_projection_test.gleam`. Construct ledger records for seen, started, completed, and acked commands and assert the projection returns the expected receipt state.

3. Implement or extend `src/scherzo/state/projection.gleam` to expose command receipts by comment id.

4. Update `test/linear_command_transport_test.gleam`. Add `acked_receipt_skips_comment_test`, `completed_unacked_receipt_posts_ack_without_submit_test`, `started_uncompleted_receipt_posts_unknown_ack_test`, and `unseen_old_comment_can_submit_test`.

5. Modify `src/scherzo/control/linear_transport.gleam` to use receipt state instead of startup-time filtering. Preserve existing parser, authorization, code-fence, max-comments-per-tick, and dedupe behavior.

6. Add daemon ledger tests in `test/orchestrator_daemon_linear_command_receipt_test.gleam`. Process one `/scherzo park` comment and assert records are appended in order: seen, started, completed, acked.

7. Implement daemon receipt appends around command execution.

8. Add `command_not_executed_if_started_receipt_append_fails_test`: fake the ledger writer to fail on `LinearCommandStarted`; assert the command handler is not called and a rejection/error is logged.

9. Add `completed_unacked_command_replays_ack_after_restart_test`: prewrite completed command receipt without ack, start daemon, and assert fake Linear command client receives an ack body without command handler invocation.

10. Add `started_uncompleted_command_gets_unknown_ack_after_restart_test`: prewrite started without completed, start daemon, and assert an unknown-outcome ack is posted and the command handler is not invoked.

11. Add `comment_posted_while_down_is_processed_when_observed_test`: prewrite recovered observed issue state, feed a Linear comment created before daemon startup but absent from receipts, and assert it submits once.

12. Add `comment_outside_bounded_poll_is_not_scanned_test`: prove the transport only processes comments returned by the bounded fake client and logs when page size equals `poll_limit_per_issue` if that warning is implemented.

13. Implement ack replay using either startup side effects or next-poll processing. Keep it bounded to observed issue ids.

14. Update README `Linear command comments` and `Implemented coverage/current limits` sections.

15. Run `direnv exec . gleam format`, `direnv exec . gleam format --check src test`, and `direnv exec . gleam test`. Record final pass count.

16. Optional credential-gated validation: start daemon with fake pi and Linear commands enabled, post a command, kill/restart between command application and ack by using a test seam or manual interruption, and verify no duplicate application occurs. If manual crash timing is impractical, record deterministic test coverage instead.

17. Commit the phase with a message such as `Persist Linear command receipts`.

## Testing and Falsifiability

This plan is falsified if a processed command comment can execute twice after restart, if a command posted while Scherzo is down is always ignored despite being on an observed issue and within bounded polling results, if completed-unacked commands are re-executed instead of acknowledged, if started-uncompleted commands are re-executed after restart, if ack failures lose the command result, or if full command bodies/secrets are stored in the ledger.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

No deterministic test may require real Linear or real pi. Use fake Linear comment clients and fake ledger writers.

## Validation and Acceptance

Accept this phase when:

- Command receipt states are durable and replayed from the ledger.
- Acked comments are skipped after restart.
- Completed-unacked commands post/retry acknowledgement without reapplying.
- Started-uncompleted commands produce `unknown_after_restart` acknowledgement without reapplying.
- Unseen commands created while down can process when observed and returned by bounded polling.
- Existing parser/authorization behavior is unchanged.
- README documents remaining bounded-polling and no-webhook limitations.
- The full deterministic suite passes.

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

Extend the command receipt projection with types equivalent to:

    pub type CommandReceiptState {
      CommandUnseen
      CommandSeen
      CommandStarted(command_name: String, issue_id: String)
      CommandCompleted(result: command.CommandResult, acked: Bool)
    }

    pub fn command_receipt(
      projection: projection.Projection,
      comment_id: String,
    ) -> CommandReceiptState

Update `linear_transport.process_comments` to accept the receipt projection or a lookup function:

    pub fn process_comments(
      state: TransportState,
      config: domain.LinearCommandConfig,
      comments: List(linear.LinearComment),
      issue_sessions: Dict(String, String),
      receipt_for: fn(String) -> projection.CommandReceiptState,
    ) -> #(TransportState, List(TransportAction))

No new package dependency should be required. This plan depends on the ledger and recovery APIs from hardening plans 02 and 03.
