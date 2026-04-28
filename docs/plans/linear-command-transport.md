# Add Linear comment command transport for Scherzo

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, an authorized human can control Scherzo from the Linear issue thread by writing explicit commands such as `/scherzo retry`, `/scherzo prompt "please continue with the smaller fix"`, `/scherzo stop`, `/scherzo park --reason waiting-for-review`, or `/scherzo ui respond <request-id> --cancel`. Scherzo polls Linear for new command comments on issues it is already observing, authorizes the Linear actor, deduplicates each comment, maps the command into the same operator command model used by `scherzoctl`, executes it through the daemon, and posts an acknowledgement comment with the command status.

This plan makes Linear an asynchronous command client for Scherzo. It does not define how Scherzo posts final task results back to Linear, does not mirror the full session transcript into comments, and does not edit issue descriptions. Those are separate Scherzo-to-Linear communication concerns.

## Problem Framing and Constraints

Scherzo already treats Linear issues as declarative work input: an issue in an eligible project and state can become work for the scheduler. Operators also need issue-local follow-up controls, especially when they are already discussing the task in Linear. A local terminal command such as `scherzoctl prompt <session> "..."` is useful, but it requires the operator to know the daemon host, control file, and session id. Linear comments are more natural for issue-scoped commands such as retrying the current issue, adding follow-up context, stopping the current worker, or parking the issue until a human is ready.

Linear is not a local trusted synchronous CLI. It is an asynchronous, polled, multi-user issue tracker. Comments can be duplicated in poll results, edited after creation, written by unauthorized users, written while Scherzo is down, or written on issues that are parked, running, terminal, or not currently observed. The transport must not parse arbitrary human discussion, must not execute edited comments repeatedly, must not bypass daemon safety checks, and must not invent command semantics that differ from `scherzoctl`.

This plan depends on the mutating operator-controls phase defining a transport-independent command model and daemon command handler. The Linear transport is only a client adapter around that model.

## Strategy Overview

Add a Linear command transport under `src/scherzo/control/linear_transport.gleam` plus a pure parser under `src/scherzo/control/linear_parser.gleam`. The parser only recognizes command lines that begin with the configured prefix, initially `/scherzo`, at the start of a comment line after trimming leading whitespace. It ignores ordinary comments, markdown paragraphs that merely mention Scherzo, and edited comments that were already processed.

Extend Linear support in `src/scherzo/linear.gleam` with bounded issue-comment queries and comment-author normalization. The daemon already polls Linear for candidates and running issue states. This transport should poll command comments only for issues the daemon is already observing in that tick: running issue ids, retry issue ids, parked issue ids, and candidate issue ids fetched during normal polling. It should not scan the whole project or historical terminal issue list.

Add a small runtime command inbox to the daemon. On each poll tick, after candidate/running issue discovery and before dispatching candidates, the daemon asks the Linear command transport for new authorized commands for observed issue ids. Each accepted command is submitted to the same daemon command handler used by the local control API. The transport records the Linear comment id in an in-memory processed set and posts an acknowledgement comment containing the command id, parsed command name, target issue/session when known, and command result status.

The first version is intentionally safe and runtime-only. It processes only comments created after the daemon started, keeps the processed-comment set in memory, ignores edits to already-processed comments, and requires an explicit allowlist of Linear user ids or emails when the transport is enabled. A future durable command receipt store or Linear webhook can improve down-time behavior and restart deduplication.

## Alternatives Considered

One alternative is to implement `/scherzo` comment handling directly inside the Linear tracker client. That would mix issue discovery with command semantics and make it tempting to mutate scheduler state from tracker code. The transport belongs at the daemon/control boundary, not inside the read tracker abstraction.

Another alternative is to implement Linear-specific command behavior separate from `scherzoctl`. That would create drift and duplicated safety logic. A retry from Linear must mean the same thing as a retry from local control, and both must pass through the same daemon checks.

A third alternative is to use Linear labels or state changes as the first command transport. Labels and states are useful declarative signals, but they are less expressive for prompt text, UI responses, and audit comments. This plan starts with explicit comments and leaves labels/state as future declarative inputs.

A fourth alternative is to add webhooks first. Webhooks would reduce polling latency and avoid repeated comment queries, but they require public endpoint configuration, signature verification, delivery retries, and a deployment story. Polling the already-observed issue set is smaller and works in the same local daemon deployment model.

A fifth alternative is to process any comment that mentions `@scherzo`. That is too ambiguous and unsafe for the first version. Commands must use a precise prefix and grammar.

## Risks and Countermeasures

The main safety risk is executing commands from unauthorized Linear users. Countermeasure: the transport is disabled by default, enabling it requires at least one authorized user id or email, and every parsed command checks the comment author before execution. Unauthorized command comments are either ignored with a log entry or acknowledged with a rejection comment, depending on configuration.

The main idempotency risk is executing the same comment on every poll. Countermeasure: the daemon stores processed Linear comment ids in memory and ignores repeated poll results. Acknowledgement comments include the source comment id and command status. The first version processes only comments whose `createdAt` is at or after daemon startup to avoid replaying old commands after restart. Durable receipts are explicitly deferred.

The main edited-comment risk is a user editing `/scherzo retry` into `/scherzo stop` after Scherzo already processed it. Countermeasure: edits to processed comments are ignored. If a user wants a new command, they must post a new comment. Tests must prove that a changed `body` with the same comment id does not execute again.

The main parsing risk is treating ordinary discussion as a command. Countermeasure: parse only lines beginning with the configured prefix after whitespace trim, require known command names, and reject malformed commands with a stable error. Do not parse commands inside markdown code fences in the first version.

The main scheduling risk is a command changing runtime state while candidate dispatch for the same issue is already in progress. Countermeasure: command polling and execution happen before candidate dispatch in the poll tick. Daemon command handlers still recheck current runtime state and return `rejected`, `not_found`, or `queued` when the timing is no longer valid.

The main noise risk is posting too many acknowledgement comments. Countermeasure: acknowledgement comments are concise and one-per-command. The body includes the source comment id, command name, status, and short message. It does not quote large prompt text. Operators can disable success acknowledgements only in a later plan if noise proves unacceptable; the first version keeps a clear audit trail.

The main privacy risk is echoing sensitive prompt text or issue content into acknowledgement comments. Countermeasure: acks truncate and redact command arguments. The actual prompt text may be sent to the worker, but the ack should include only a short excerpt. The transport uses the same secret redaction list as daemon logging.

The main coverage risk is commands on issues Scherzo is not observing. Countermeasure: document the first-version boundary clearly. Scherzo polls command comments only for running, retrying, parked, and candidate issues. Commands on terminal or unrelated issues are not observed until a future webhook or broader bounded search exists.

## Progress

- [x] (2026-04-28 23:05Z) Created this plan to split Linear-to-Scherzo commands from Scherzo-to-Linear result reporting.
- [ ] Normalize the tree after `docs/plans/mutating-operator-controls.md` is complete and the shared command model exists.
- [ ] Add Linear command configuration and pure parser.
- [ ] Add bounded Linear comment query/read APIs and fake tests.
- [ ] Wire command polling into the daemon poll tick before candidate dispatch.
- [ ] Submit parsed commands through the shared daemon command handler.
- [ ] Add acknowledgement comments, idempotency, authorization, and edited-comment tests.
- [ ] Document Linear command syntax and limitations.

## Surprises & Discoveries

- Observation: The current tracker abstraction only reads candidate issues, issues by state, and issue states by id; it does not expose comments.
  Evidence: `src/scherzo/tracker.gleam` defines `fetch_candidate_issues`, `fetch_issues_by_states`, and `fetch_issue_states_by_ids` only.

- Observation: Scherzo already has Linear comment creation helpers for handoff, but no comment-reading helpers.
  Evidence: `src/scherzo/linear.gleam` defines `build_comment_create_request` and `comment_create_mutation`; no comment query builder exists yet.

- Observation: The current outbound handoff comments are operational and do not imply a command transport.
  Evidence: `src/scherzo/handoff.gleam` posts claim, success, and failure comments, but it never reads Linear comments or interprets human text.

## Decision Log

- Decision: Treat Linear comments as an asynchronous transport into the same command model used by `scherzoctl`.
  Rationale: This keeps scheduler mutations centralized and avoids diverging semantics between local and Linear operation.
  Date: 2026-04-28

- Decision: Process only explicit prefix commands such as `/scherzo retry` in the first version.
  Rationale: Arbitrary comment parsing or `@scherzo` mentions are too ambiguous and easy to trigger accidentally.
  Date: 2026-04-28

- Decision: Require an explicit author allowlist when Linear commands are enabled.
  Rationale: Linear issue visibility is not the same as permission to control a live daemon. The first version should fail closed.
  Date: 2026-04-28

- Decision: Use polling of already-observed issue ids before adding webhooks.
  Rationale: It fits the current daemon deployment model and avoids public endpoint/signature work before the command semantics are proven.
  Date: 2026-04-28

- Decision: Ignore edits to already-processed command comments.
  Rationale: Comment edits are hard to make idempotent and auditable. Requiring a new command comment is simpler and safer.
  Date: 2026-04-28

- Decision: Do not persist command receipts in this phase.
  Rationale: Runtime-only dedupe is enough to prevent repeated execution during one daemon run. Durable receipts require storage and migration design that should not block the first transport.
  Date: 2026-04-28

## Outcomes & Retrospective

(To be filled at completion. Include final command grammar, real Linear query shape, whether polling latency was acceptable, and any commands intentionally left unsupported from Linear.)

## Context and Orientation

Scherzo is a Gleam Erlang-target project. Runtime source lives under `src/scherzo/`, tests live under `test/`, and validation is run from the repository root with `direnv exec . gleam test`. The daemon actor in `src/scherzo/orchestrator/daemon.gleam` owns scheduling state, worker handles, retry timers, workflow reload state, EventHub publication, and Linear handoff side effects.

The local control plans introduce a `scherzoctl` client and, in the mutating-controls phase, a shared command model under `src/scherzo/control/command.gleam`. This plan adds a second command transport: Linear comments. A command transport authenticates/parses input, maps it to `control/command.OperatorCommand`, submits it to the daemon command handler, and reports the `control/command.CommandResult` back to the caller through transport-appropriate means.

Linear handoff is outbound reporting from Scherzo to Linear. It is implemented in `src/scherzo/handoff.gleam` and writes claim/success/failure comments or state updates. Linear command transport is inbound control from Linear to Scherzo. It reads human comments and submits commands. These are separate surfaces and must remain separately testable.

An observed issue is an issue id Scherzo already knows about during a daemon poll: currently running issues, issues with retry timers, parked issues, and candidate issues fetched from Linear active states. The first command transport polls comments only for observed issues.

## Preconditions and Verified Facts

Before implementing this plan:

- `docs/plans/real-board-readiness.md` is complete.
- `docs/plans/session-eventhub.md` is complete.
- `docs/plans/local-control-api-and-scherzoctl.md` is complete.
- `docs/plans/mutating-operator-controls.md` is complete.
- `src/scherzo/control/command.gleam` exists and defines the shared command model.
- The daemon exposes a command handler or message that accepts `control/command.OperatorCommand` and returns `control/command.CommandResult`.
- `scherzoctl` mutating commands work locally and publish audit events.
- `src/scherzo/linear.gleam` can create Linear comments through `build_comment_create_request` and `parse_mutation_response`.
- `direnv exec . gleam test` passes.

Current repository facts at plan-authoring time:

- `src/scherzo/domain.gleam` defines `HandoffConfig`, but no Linear command config.
- `src/scherzo/tracker.gleam` does not expose comments.
- `src/scherzo/linear.gleam` fetches issues and writes comments/state updates, but does not read comments.
- `docs/TODO.md` still tracks Scherzo-to-Linear final result reporting separately from this inbound command plan.

If mutating controls are not complete, do not implement this plan. Implementing Linear comments first would force this plan to invent a second command path.

## Scope Boundaries

In scope: Linear command configuration; command comment parser; bounded Linear issue-comment query helpers; author allowlist checks; in-memory processed-comment dedupe; daemon integration that polls observed issue comments before candidate dispatch; command submission through the shared daemon command handler; acknowledgement comments; tests for parsing, authorization, idempotency, edited-comment behavior, command ordering, and ack formatting; README documentation for command syntax and safety boundaries.

Out of scope: Scherzo-to-Linear final result comments; mirroring all model output into Linear; editing issue descriptions; parsing arbitrary comments or mentions; Linear webhooks; durable command receipt storage; role-based permissions beyond explicit allowlists; commands on unrelated or terminal historical issues; using labels or issue state changes as imperative commands; multi-daemon distributed command coordination.

## Milestones

Milestone 1 defines config and parsing. At the end, tests can parse valid `/scherzo` commands into the same `control/command.OperatorCommand` values used by `scherzoctl`, reject malformed commands, ignore non-command text, and enforce code-fence ignoring.

Milestone 2 adds Linear comment read support. At the end, pure and fake-transport tests can build bounded GraphQL requests for issue comments, parse comment ids, bodies, timestamps, edited timestamps, and author id/email/name, and map API errors without leaking the API key.

Milestone 3 implements authorization and dedupe in isolation. At the end, a transport state object can receive comment batches, ignore old comments, reject unauthorized comments, ignore already-processed comment ids, ignore edited repeats, and produce command submissions plus acknowledgement bodies.

Milestone 4 wires the transport into the daemon. At the end, daemon tests show that command polling runs for observed issue ids before candidate dispatch and that parsed commands call the shared command handler rather than any Linear-specific mutation code.

Milestone 5 adds acknowledgement comments and documentation. At the end, successful, queued, rejected, unauthorized, and malformed commands produce concise Linear ack comments when configured, README documents the syntax, and all deterministic tests pass.

## Plan of Work

Extend `src/scherzo/domain.gleam` with `LinearCommandConfig`. Include `enabled`, `prefix`, `authorized_user_ids`, `authorized_user_emails`, `acknowledge_success`, `acknowledge_rejection`, `poll_limit_per_issue`, and `ignore_comments_before_ms` or an equivalent runtime daemon-start watermark. Add it to `domain.EffectiveConfig` so reloads can enable or disable command polling. The default is disabled. When enabled, config validation must require at least one authorized user id or email.

Extend `src/scherzo/config.gleam` to parse top-level `linear_commands:` or `operator_controls.linear:`. Choose one spelling and document it; this plan recommends top-level `linear_commands` because the feature is explicitly Linear-specific. Validate non-empty prefix, positive poll limit, and non-empty authorization when enabled.

Create `src/scherzo/control/linear_parser.gleam`. It exposes a parser that takes a comment body, the source issue id/identifier, and optionally the current session id for that issue, then returns `Option(ParsedLinearCommand)` or a structured parse error. It must parse only lines beginning with the prefix after trimming whitespace. It should ignore lines inside triple-backtick fenced code blocks.

Supported first-version command grammar:

    /scherzo help
    /scherzo status
    /scherzo retry
    /scherzo park --reason <text>
    /scherzo unpark
    /scherzo stop
    /scherzo abort
    /scherzo stop-after-turn
    /scherzo prompt <text>
    /scherzo continue <text>
    /scherzo ui respond <request-id> --cancel
    /scherzo ui respond <request-id> --value <text>

`retry`, `park`, and `unpark` target the comment's issue by identifier or id. `stop`, `abort`, `stop-after-turn`, `prompt`, `continue`, and `ui respond` target the current running session for that issue; if no running session exists, the daemon command handler returns `not_found` or `rejected`.

Create `src/scherzo/control/linear_transport.gleam`. It owns transport state for `processed_comment_ids`, `daemon_started_at_ms`, and configuration. It receives normalized comments from Linear, filters by created time, authorization, and dedupe, parses commands, and returns actions: submit command, post acknowledgement, or log ignore/rejection. The module should be testable without a daemon or network.

Extend `src/scherzo/linear.gleam` with `LinearComment` and request helpers. Add a GraphQL query that fetches recent comments for a list of issue ids or for one issue id at a time with `first: poll_limit_per_issue`. Normalize fields: comment id, issue id, body, createdAt, updatedAt, author id, author email, and author name. Keep the helper bounded and avoid unbounded pagination in this phase.

Add a production `linear_commands` client wrapper that uses `linear.http_transport` and `linear.build_comment_create_request` for acknowledgement comments. It should not be added to `tracker.Client`; keep issue tracking and command comments as separate interfaces.

Modify `src/scherzo/orchestrator/daemon.gleam`. On each poll tick, after it knows the observed issue ids for the tick and before dispatching candidates, ask the command transport to fetch comments for those issue ids if `linear_commands.enabled` is true. For each parsed command, call the shared daemon command handler. For each result, publish an audit event and enqueue an acknowledgement comment side effect if configured.

Acknowledgement comments should be concise. Example:

    Scherzo command received from comment <comment-id>.
    Command: prompt
    Status: queued
    Target: session ABC-123-run-42
    Run: ABC-123--576460751521

Do not quote full prompt text. Include a short redacted/truncated excerpt only when useful.

Update README with a `Linear command comments` section. Document enabling config, authorization, command syntax, idempotency behavior, ignored edits, polling boundary, and the fact that final task results are not covered by this plan.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current pass count. Stop if mutating control tests fail.

2. Add `test/linear_command_config_test.gleam`. Test that default config disables Linear commands, enabled config requires at least one authorized user id or email, custom prefix parses, and invalid poll limits fail.

3. Extend `src/scherzo/domain.gleam` and `src/scherzo/config.gleam` with `LinearCommandConfig`. Update direct `EffectiveConfig` constructors in tests. Run `direnv exec . gleam test`.

4. Create `test/linear_command_parser_test.gleam`. Add tests for `/scherzo retry`, `/scherzo park --reason waiting`, `/scherzo unpark`, `/scherzo stop`, `/scherzo prompt please continue`, `/scherzo continue please use the smaller fix`, `/scherzo ui respond ui-1 --cancel`, and `/scherzo ui respond ui-1 --value approved`.

5. In the same parser test file, add negative cases: ordinary comments are ignored, `/not-scherzo retry` is ignored, unknown command returns a parse error, malformed UI response returns a parse error, and commands inside triple-backtick code fences are ignored.

6. Implement `src/scherzo/control/linear_parser.gleam` until parser tests pass. Parsed mutating commands must contain `control/command.OperatorCommand` values.

7. Add `test/linear_comments_test.gleam`. Test `build_issue_comments_request` includes a bounded limit, issue ids, Authorization header, and no Bearer prefix. Test parsing a fake response with two comments including author id/email/name and created/updated timestamps.

8. Extend `src/scherzo/linear.gleam` with comment query builders and parsers. Run tests.

9. Create `test/linear_command_transport_test.gleam`. Add `authorized_new_comment_submits_command_and_ack_test`: feed one normalized comment from an authorized author, assert the transport emits one command submission and one ack action.

10. Add `unauthorized_comment_is_rejected_without_command_test`: feed a command from an unauthorized author and assert no command submission. If rejection acks are configured, assert a rejection ack action is produced.

11. Add `processed_comment_id_is_not_executed_twice_test`: feed the same comment id twice and assert only one command submission.

12. Add `edited_processed_comment_is_ignored_test`: feed comment id `c1` with `/scherzo retry`, then feed `c1` with `/scherzo stop` and a newer updated timestamp. Assert only the retry command was submitted.

13. Add `old_comment_before_daemon_start_is_ignored_test`: set daemon start watermark to 1000 ms, feed comment created at 900 ms, and assert it is ignored.

14. Implement `src/scherzo/control/linear_transport.gleam` until transport tests pass.

15. Add daemon tests in `test/orchestrator_daemon_linear_command_test.gleam`. Use fake Linear comments and fake command handler plumbing. Assert command polling uses observed issue ids consisting of running, retry, parked, and candidate issues, and does not query unrelated historical issue ids.

16. Add `linear_commands_run_before_candidate_dispatch_test`: arrange a candidate issue with a `/scherzo park` comment, run a poll, and assert the issue is parked before dispatch eligibility is evaluated.

17. Add `linear_prompt_comment_targets_current_session_test`: arrange a running session for issue `ABC-1`, feed `/scherzo prompt continue`, and assert the daemon command handler receives `PromptSession(<current-session-id>, "continue")`.

18. Implement daemon integration for Linear command polling. Keep network operations as side effects, use fake dependencies in tests, and do not block worker event publication on slow comment polling.

19. Add ack comment formatting tests. Assert `applied`, `queued`, `rejected`, `not_found`, `not_allowed`, parse errors, and unauthorized errors produce short comments containing the source comment id and no API key or full prompt text.

20. Wire ack comments through Linear comment creation side effects. Reuse `linear.build_comment_create_request` and `linear.parse_mutation_response`. Handoff failure behavior is a guide: ack-posting failure logs a warning and does not undo an already-applied command.

21. Update README with config and command examples. Include the warning that comments posted while Scherzo is down are not processed in this first version because old comments are ignored on startup.

22. Run `direnv exec . gleam format`, `direnv exec . gleam format --check src test`, and `direnv exec . gleam test`. Record final pass count in Progress.

23. Optional credential-gated validation: against a private Linear test issue, enable `linear_commands` for one authorized test user, start daemon mode with fake pi, post `/scherzo status` and `/scherzo prompt hello from Linear`, and verify Scherzo posts ack comments and the fake worker receives the prompt only once.

24. Commit the phase with a message such as `Add Linear command transport`.

## Testing and Falsifiability

The parser is falsified if ordinary discussion triggers commands, if code-fenced examples execute, if unknown commands are silently ignored instead of producing a parse error for explicit `/scherzo` lines, or if parsed command values differ from the corresponding `scherzoctl` command values.

Authorization is falsified if an enabled transport can execute a command from a user not in the allowlist. Idempotency is falsified if the same Linear comment id can execute twice during one daemon run or if editing a processed command changes the executed command.

Daemon integration is falsified if Linear commands mutate scheduler state outside the shared command handler, if command polling happens after candidate dispatch and cannot stop same-tick dispatch, if command polling scans unrelated historical issues, if ack failures crash the daemon, or if slow command polling blocks worker event handling.

Add deterministic tests in `test/linear_command_config_test.gleam`, `test/linear_command_parser_test.gleam`, `test/linear_comments_test.gleam`, `test/linear_command_transport_test.gleam`, `test/orchestrator_daemon_linear_command_test.gleam`, and relevant `test/linear_test.gleam` mutation/parse tests. No deterministic test may require real Linear, real pi, or network access.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Before implementation, tests importing `scherzo/control/linear_parser` and comment query helpers should fail to compile. After implementation, the full suite should pass without hangs.

## Validation and Acceptance

Accept deterministic implementation when:

- Linear command config is disabled by default and fails closed without an allowlist.
- Parser tests cover all supported command syntax and negative cases.
- Linear comment query helpers are bounded and parse author identity.
- The transport deduplicates by comment id and ignores processed edits.
- Daemon command polling applies commands before candidate dispatch.
- All submitted commands pass through the shared daemon command handler.
- Ack comments are concise, redacted, and reference the source comment id.
- `direnv exec . gleam test` passes.

Credential-gated manual acceptance with a private Linear issue:

1. Enable Linear commands for one test user id or email.
2. Start Scherzo daemon with fake pi and one active test issue.
3. Post `/scherzo status` and expect a Scherzo ack comment without worker mutation.
4. Post `/scherzo prompt hello from Linear` while a fake worker is running and expect exactly one queued/applied ack.
5. Verify the fake-pi transcript receives the prompt at most once.
6. Edit the original prompt comment to `/scherzo abort` and verify Scherzo ignores the edit.
7. Post `/scherzo abort` as a new comment and verify the command is authorized, executed, and acknowledged.

## Rollout, Recovery, and Idempotence

Roll out with `linear_commands.enabled: false` by default. Enable it only on a private test project with one authorized user first. Keep local `scherzoctl` controls available as the fallback if Linear command polling behaves unexpectedly.

If acknowledgement comment creation fails, the command result remains applied or rejected according to the daemon handler. Scherzo logs the ack failure and continues. Operators can use EventHub attach or local logs to verify the command result.

If the daemon restarts, it loses the in-memory processed-comment set and start watermark. The first version intentionally ignores comments older than daemon startup time, so old commands are not replayed but commands posted while Scherzo was down are missed. A future durable command receipt store or webhook delivery path can change this behavior explicitly.

If a command is malformed, unauthorized, or rejected, Scherzo does not mutate runtime state. It may post a rejection acknowledgement if configured. Users must post a new comment to retry; edits are ignored after processing.

## Artifacts and Notes

Example config:

    linear_commands:
      enabled: true
      prefix: "/scherzo"
      authorized_user_emails:
        - operator@example.com
      poll_limit_per_issue: 25
      acknowledge_success: true
      acknowledge_rejection: true

Example comments:

    /scherzo retry
    /scherzo prompt Please apply the smaller migration strategy we discussed above.
    /scherzo stop-after-turn
    /scherzo ui respond ui-17 --cancel

Example acknowledgement:

    Scherzo command received from comment 3f2c...
    Command: prompt
    Status: queued
    Target: session LIV-9--576460751521-1

This plan intentionally does not decide how final pi responses or task summaries are posted to Linear. That belongs to a separate Scherzo-to-Linear session results plan.

## Interfaces and Dependencies

In `src/scherzo/domain.gleam`, add a config type equivalent to:

    pub type LinearCommandConfig {
      LinearCommandConfig(
        enabled: Bool,
        prefix: String,
        authorized_user_ids: List(String),
        authorized_user_emails: List(String),
        poll_limit_per_issue: Int,
        acknowledge_success: Bool,
        acknowledge_rejection: Bool,
      )
    }

and add it to `EffectiveConfig`, or place it under a broader operator-controls config if that already exists after the mutating-controls phase.

In `src/scherzo/linear.gleam`, add types equivalent to:

    pub type LinearCommentAuthor {
      LinearCommentAuthor(
        id: String,
        email: Option(String),
        name: Option(String),
      )
    }

    pub type LinearComment {
      LinearComment(
        id: String,
        issue_id: String,
        body: String,
        created_at_ms: Int,
        updated_at_ms: Int,
        author: LinearCommentAuthor,
      )
    }

    pub fn build_issue_comments_request(
      config: domain.TrackerConfig,
      issue_ids: List(String),
      limit_per_issue: Int,
    ) -> Result(Request, error.TrackerError)

    pub fn parse_comments_response(
      response: Response,
    ) -> Result(List(LinearComment), error.TrackerError)

If Linear's GraphQL API cannot fetch comments for multiple issue ids in one bounded query, implement one bounded request per issue id and keep the daemon side-effect queue from growing without limit.

In `src/scherzo/control/linear_parser.gleam`, expose:

    pub type ParseError {
      UnknownCommand(String)
      MissingArgument(String)
      InvalidArgument(String)
      NoCurrentSession(String)
    }

    pub type ParsedLinearCommand {
      ParsedLinearCommand(
        source_issue_id: String,
        source_comment_id: String,
        command: command.OperatorCommand,
        excerpt: String,
      )
    }

    pub fn parse_comment(
      prefix: String,
      source_issue_id: String,
      source_issue_identifier: String,
      current_session_id: Option(String),
      comment_id: String,
      body: String,
    ) -> Result(Option(ParsedLinearCommand), ParseError)

In `src/scherzo/control/linear_transport.gleam`, expose pure processing helpers equivalent to:

    pub type TransportState

    pub type TransportAction {
      SubmitCommand(comment: linear.LinearComment, parsed: linear_parser.ParsedLinearCommand)
      PostAck(issue_id: String, body: String)
      LogIgnored(reason: String, comment_id: String)
    }

    pub fn new_state(daemon_started_at_ms: Int) -> TransportState

    pub fn process_comments(
      state: TransportState,
      config: domain.LinearCommandConfig,
      comments: List(linear.LinearComment),
      issue_sessions: Dict(String, String),
    ) -> #(TransportState, List(TransportAction))

No new package dependency should be required. Use existing JSON, Linear transport, daemon actor, EventHub, and control command modules.
