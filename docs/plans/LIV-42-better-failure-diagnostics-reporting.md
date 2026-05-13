# Add actionable failure diagnostics to Linear handoff comments

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

When a Scherzo workflow fails today, the Linear issue receives a short comment such as `Scherzo failed run LIV-30--576460690849-1 for LIV-30 with error agent_pi_failed.` That proves that Scherzo noticed the failure, but it does not tell an operator whether pi exited, timed out, returned malformed JSON, hit a blocked UI request, failed a hook, or failed before the workspace was prepared. After this change, the Linear failure comment will still be safe to post on the issue, but it will include enough redacted, bounded, structured diagnostics for an operator to choose the next debugging action without first finding daemon logs.

The observable outcome is a Linear comment that keeps the current header and adds a `Failure diagnostics` section containing the top-level Scherzo agent error code, the nested error code when there is one, a short redacted detail message when the error carries one, and safe run metadata such as token totals and a non-absolute workspace path when available.

## Problem Framing and Constraints

The user-facing problem is operational ambiguity. A failed run currently reports only the top-level `error.agent_code(failure.reason)` value, for example `agent_pi_failed`. That code identifies the broad subsystem, but it hides the more useful nested cause such as `pi_protocol_error`, `pi_exited`, `pi_turn_timeout`, `hook_failed`, or `template_render_error`. Operators cannot tell whether to retry, inspect pi, fix a prompt template, fix a workspace hook, or search logs.

The change must be small and proportionate. The ticket asks for better reporting on the issue, not a new diagnostics database, a log aggregation system, or a full failure artifact attachment flow. The plan therefore keeps the existing Linear handoff path and adds a formatter for failure comments.

The change must not leak secrets. Failure detail strings can include subprocess output, malformed JSON, hook output, API status text, or pi protocol messages. The existing success comment path already redacts configured tracker secrets through `log.redact`; failure comments must use the same kind of redaction and must also bound long values so that a single failure cannot flood Linear. This plan does not attempt perfect secret detection beyond Scherzo's existing configured-secret redaction. [CLARIFY] Confirm whether failure comments may include arbitrary nested error detail after configured-secret redaction, or whether some error families should publish only codes and send users to daemon logs for raw detail.

This repository is a Gleam/Erlang project. Commands in this plan assume they are run from the repository root. The repository uses `direnv`; if `direnv exec . <command>` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry the command through `direnv exec .`.

## Strategy Overview

Add a single failure-comment formatting function next to the existing success-comment formatting in `src/scherzo/handoff_format.gleam`, then call that function from `src/scherzo/handoff.gleam` inside `report_failure`. The formatter should convert `runner.WorkerFailure.reason` into a stable operator-facing diagnostic block. It should reuse the existing error-code functions in `src/scherzo/error.gleam`, redact with `log.redact`, truncate details, and avoid publishing absolute workspace paths.

This is the right size because the failure data already exists in `runner.WorkerFailure` and the nested error types in `src/scherzo/error.gleam`. The missing behavior is presentation at the Linear handoff boundary. Keeping the logic in `handoff_format.gleam` also mirrors the existing `success_comment` and `success_result_attachment_markdown` code, so the change remains easy to test without mocking the whole daemon.

## Alternatives Considered

The simplest alternative is to append only the nested error code to the existing one-line string, for example `with error agent_pi_failed:pi_protocol_error`. That would help, but it would still hide important messages such as a hook command name and exit status, a template-render error, a pi protocol rejection reason, or a launch failure message. It also would not create a clear place for token totals or workspace metadata.

Another option is to attach a full failure artifact file to the Linear comment, similar to success result attachments. That is larger than this ticket requires. It introduces upload behavior, fallback links, and attachment lifecycle concerns for every failure, while the immediate operator pain can be solved by a short bounded comment.

A third option is to teach the daemon logs or local control API to expose richer failure diagnostics. That may be useful later, but it does not solve the specific issue: the Linear comment is where operators first see that a workflow failed.

## Risks and Countermeasures

The main risk is secret leakage. The countermeasure is to centralize all failure text construction in `handoff_format.failure_comment`, pass tracker secrets from `src/scherzo/handoff.gleam`, call `log.redact` on the complete comment body, and test that a detail containing the configured Linear API key is redacted. Long raw details must be truncated before or during formatting. This reduces but does not eliminate the risk of unknown secrets in arbitrary subprocess output; that limitation is called out as a clarification.

Another risk is comment noise. Operators need actionable context, not a stack dump. The countermeasure is a fixed, short Markdown section with stable field names and a maximum detail length. Do not include raw JSON blobs, full stdout, full stderr, or all pi RPC records.

Another risk is exposing local filesystem paths in Linear. `WorkerFailure.workspace_path` may be useful when it is repository-relative, but an absolute path can reveal usernames or host layout. The countermeasure is to include the workspace only when the string is present and not absolute. If the value starts with `/`, emit `workspace: _not shown because Scherzo recorded an absolute path_` or omit the workspace line.

Another risk is breaking existing tests that intentionally asserted that arbitrary detail text was not present. The change is expected to make selected detail text present. Update tests to assert redaction and boundedness rather than absence of all detail.

## Progress

- [x] (2026-05-02 00:00Z) Read the repository-local exec-plan authoring skill and confirmed this plan must be self-contained and must include progress, discoveries, decisions, outcomes, and open questions.
- [x] (2026-05-02 00:00Z) Inspected the current failure handoff path and identified `src/scherzo/handoff.gleam`, `src/scherzo/handoff_format.gleam`, `src/scherzo/error.gleam`, `src/scherzo/agent/runner.gleam`, `src/scherzo/agent/pi_rpc.gleam`, `src/scherzo/port.gleam`, `test/handoff_test.gleam`, `gleam.toml`, and `README.md` as the relevant implementation surfaces.
- [x] (2026-05-02 22:48Z) Revised the workspace-path test instructions to remove literal local absolute path examples while preserving the requirement to test slash-prefixed workspace values.
- [x] (2026-05-02 22:49Z) Ran `scripts/scherzo-execplan validate`; validation passed with `PLAN_PATH=docs/plans/LIV-42-better-failure-diagnostics-reporting.md` and `VALIDATION=ok`.
- [x] (2026-05-02 21:45Z) Implemented `handoff_format.failure_comment` in `src/scherzo/handoff_format.gleam` with nested error codes, bounded details, token totals, redaction, and safe workspace display.
- [x] (2026-05-02 21:45Z) Replaced the inline failure comment string in `src/scherzo/handoff.gleam` with the formatter.
- [x] (2026-05-02 21:45Z) Added and updated focused failure handoff tests in `test/handoff_test.gleam` for pi, hook, redaction, truncation, and workspace path behavior.
- [x] (2026-05-02 21:45Z) Ran formatting and tests with `direnv exec . gleam format --check src test`, `direnv exec . gleam test test/handoff_test.gleam`, and `direnv exec . gleam test`; all passed.

## Surprises & Discoveries

- Observation: `src/scherzo/port.gleam` already redirects child stderr to a diagnostics file and exposes `read_diagnostics`, but the currently inspected `src/scherzo/agent/pi_rpc.gleam` failure mapping does not pass those diagnostics into `runner.WorkerFailure`.
  Evidence: the port module comment says stderr is kept out of the stdout JSONL stream and can be read through `read_diagnostics`, while `pi_rpc.map_port_error` maps `ProcessExited(status)` to `PiExited(status)` and other port failures to codes or short strings.
- Observation: the success handoff path already had a formatter and redaction boundary before implementation, but the failure path constructed the comment inline.
  Evidence: `src/scherzo/handoff_format.gleam` exported `success_comment`, while `src/scherzo/handoff.gleam` built `"Scherzo failed run " <> run_id <> ... <> error.agent_code(failure.reason)` directly in `report_failure`.
- Observation: the repository's `gleam test test/handoff_test.gleam` invocation runs the full test suite rather than only that single file.
  Evidence: the command completed successfully with `545 passed, no failures`, matching the subsequent `direnv exec . gleam test` full-suite result.
- Observation: `scripts/scherzo-execplan validate` is intended for newly added plan files and rejects this already-checked-in plan when it is modified.
  Evidence: `direnv exec . scripts/scherzo-execplan validate` exited with `scherzo-execplan: plan file must be newly added, got status M: docs/plans/LIV-42-better-failure-diagnostics-reporting.md`.

## Decision Log

- Decision: Implement better failure diagnostics as a bounded Markdown section in the existing Linear failure comment rather than as a new attachment or storage feature.
  Rationale: The issue asks specifically for the failure report on the issue to be more useful. The needed data is already available in `runner.WorkerFailure.reason`; a formatter is the smallest useful change.
  Date: 2026-05-02
- Decision: Put the formatter in `src/scherzo/handoff_format.gleam` and call it from `src/scherzo/handoff.gleam`.
  Rationale: Success comment formatting already lives in `handoff_format.gleam`; keeping failure formatting there avoids spreading Markdown construction and makes unit tests straightforward.
  Date: 2026-05-02
- Decision: Include nested error codes and short redacted detail strings, but never include raw pi RPC records, full stdout/stderr, or absolute workspace paths in the Linear comment.
  Rationale: Nested codes and short details answer the operator's immediate question while limiting comment size and privacy risk.
  Date: 2026-05-02
- Decision: Describe absolute-workspace test inputs as slash-prefixed synthetic values instead of embedding local host path literals.
  Rationale: ExecPlans must be portable and Scherzo's plan validator rejects local absolute path examples. A synthetic slash-prefixed value still tests the formatter's absolute-path suppression behavior without documenting any real host layout.
  Date: 2026-05-02

## Outcomes & Retrospective

The implementation now routes failure handoff comments through `handoff_format.failure_comment`. The generated Linear comment keeps the recognizable `Scherzo failed run` header and adds a `Failure diagnostics` section containing the top-level agent code, nested error code when present, a short redacted detail, safe workspace information, and token totals. Focused tests cover pi exit diagnostics, pi protocol redaction and truncation, hook diagnostics, and hiding slash-prefixed workspace paths. `direnv exec . gleam format --check src test`, `direnv exec . gleam test test/handoff_test.gleam`, and `direnv exec . gleam test` all pass.

## Context and Orientation

Scherzo is a Gleam/Erlang orchestration daemon that runs pi coding-agent workflows against Linear issues. Linear is the issue tracker. A handoff is Scherzo's act of posting a comment or moving an issue after claiming, succeeding, or failing a run. A pi RPC error is an error from the pi coding-agent process or its JSON-lines RPC protocol.

The failure report path begins with agent or workflow execution producing a `runner.WorkerFailure` from `src/scherzo/agent/runner.gleam`. That type has four fields: `reason`, `workspace_path`, `tokens`, and `final_issue`. The `reason` field is an `error.AgentRunnerError` from `src/scherzo/error.gleam`. `AgentRunnerError` variants include `PromptFailed`, `WorkspaceFailed`, `HookFailedError`, `ProbeFailed`, `PiFailed`, `StateRefreshFailed`, `OperatorAbort`, and `OperatorStopAfterCurrentTurn`. Several of those variants wrap more specific error types such as `PiRpcError`, `HookError`, `WorkspaceError`, or `TemplateError`.

`src/scherzo/error.gleam` currently provides stable code functions. `error.agent_code` maps an `AgentRunnerError` to broad strings such as `agent_pi_failed`, `agent_hook_failed`, and `agent_workspace_failed`. `error.pi_rpc_code` maps nested pi errors to strings such as `pi_launch_failed`, `pi_malformed_json`, `pi_read_timeout`, `pi_turn_timeout`, `pi_stall_timeout`, `pi_exited`, and `pi_protocol_error`. There are similar functions for workspace, hook, tracker, template, and subprocess errors.

`src/scherzo/handoff.gleam` constructs a Linear handoff client. For success, it delegates comment text to `handoff_format.success_comment` in `src/scherzo/handoff_format.gleam`. For failure, the initial implementation built the whole comment inline as one sentence and included only `error.agent_code(failure.reason)`. The completed implementation delegates failure comment text to `handoff_format.failure_comment` so the comment includes structured diagnostics.

`test/handoff_test.gleam` exercises the comments-only handoff path. It creates a fake Linear transport, calls `client.report_failure`, receives the generated GraphQL request body, and asserts that the body contains the run id, `agent_pi_failed`, `Failure diagnostics`, nested error codes, redacted details, bounded long details, and safe workspace behavior.

## Preconditions and Verified Facts

The repository has `gleam.toml` with package name `scherzo`, target `erlang`, and `gleeunit` as the test dependency. Validation commands should be run from the repository root with `direnv exec .` when available.

The current working tree was clean before this plan file was created, as shown by `jj status --color=never` reporting `The working copy has no changes.`

The relevant current files exist with these responsibilities:

- `src/scherzo/handoff.gleam` owns Linear handoff actions and delegates failure comment text to `handoff_format.failure_comment` in `report_failure`.
- `src/scherzo/handoff_format.gleam` owns success comment, success attachment Markdown formatting, and failure comment Markdown formatting, and imports `scherzo/log` for redaction.
- `src/scherzo/error.gleam` defines the error types and code functions that should remain the source of stable diagnostic codes.
- `src/scherzo/agent/runner.gleam` defines `WorkerFailure` and creates failures with the top-level `AgentRunnerError` reason, optional workspace path, token totals, and optional final issue.
- `src/scherzo/agent/pi_rpc.gleam` maps pi process and protocol failures into `PiRpcError` variants.
- `src/scherzo/port.gleam` wraps subprocess ports and has a diagnostics-file concept for stderr, but those diagnostics are not currently part of `WorkerFailure`.
- `test/handoff_test.gleam` has focused tests for handoff comments and redaction.
- `README.md` documents that Scherzo is YAML-orchestrator and YAML-DAG based, and shows `direnv exec . gleam run` usage.

## Scope Boundaries

In scope:

- Add failure comment formatting in `src/scherzo/handoff_format.gleam`.
- Change `src/scherzo/handoff.gleam` to use that formatter for `report_failure`.
- Add or update unit tests in `test/handoff_test.gleam` for the generated Linear comment body.
- Use existing error types, existing code functions, existing `runner.WorkerFailure` fields, and existing redaction helpers.

Out of scope:

- Do not change workflow execution semantics, retry behavior, issue state transitions, workspace lifecycle, or pi process control.
- Do not add a new Linear attachment path for failure artifacts.
- Do not add a database, durable failure log, or new local control API endpoint.
- Do not thread full daemon configuration into handoff solely to improve redaction.
- Do not include full pi RPC event history or full subprocess diagnostics in the Linear comment.
- Do not change success handoff behavior except for harmless helper reuse if necessary.

## Milestones

The first milestone creates a formatter that can be tested without touching Linear transport behavior. At the end of this milestone, `src/scherzo/handoff_format.gleam` will export a `failure_comment` function that turns a `runner.WorkerFailure` into a redacted Markdown comment. Focused tests can call the handoff client and observe the formatter indirectly through the fake transport.

The second milestone wires the formatter into the existing handoff path. At the end of this milestone, the current one-line failure comment in `report_failure` will be replaced by a structured comment, while issue state update behavior remains unchanged.

The third milestone validates redaction, truncation, and representative error families. At the end of this milestone, tests will prove that pi protocol details, pi exit statuses, hook failures, workspace failures, and prompt failures produce useful bounded diagnostics, and that known secrets do not appear in the generated GraphQL request body.

## Plan of Work

In `src/scherzo/handoff_format.gleam`, add an exported function:

    pub fn failure_comment(
      issue: domain.Issue,
      failure: runner.WorkerFailure,
      run_id: String,
      secrets: List(String),
    ) -> String

The function should build this shape:

    Scherzo failed run <run_id> for <issue.identifier>.

    Failure diagnostics:
    - error: <agent_code>
    - underlying_error: <nested_code>
    - detail: <short detail, only when available>
    - workspace: <workspace path, only when non-absolute and available>
    - tokens: input=<n> output=<n> cache_read=<n> cache_write=<n> total=<n>

The exact Markdown labels may be adjusted during implementation, but keep them stable, lowercase, and easy to grep. The first line should remain recognizable as the existing failure header so operators who search for `Scherzo failed run` still find it.

Add private helpers in `src/scherzo/handoff_format.gleam` for converting errors into fields. Do not change the constructors in `src/scherzo/error.gleam` unless a compiler exhaustiveness error forces a small helper addition. The formatter can pattern match directly on `failure.reason` and use the existing code functions. Suggested helper names are `failure_diagnostics`, `nested_error_code`, `failure_detail`, `safe_workspace_line`, `tokens_metadata`, and `truncate_detail`.

The nested-code mapping should be prescriptive:

- `error.PromptFailed(template_error)` uses `error.template_code(template_error)` as `underlying_error` and includes the template message for `TemplateRenderError(message)` as detail.
- `error.WorkspaceFailed(workspace_error)` uses `error.workspace_code(workspace_error)` as `underlying_error` and includes constructor details when present.
- `error.HookFailedError(hook_error)` uses `error.hook_code(hook_error)` as `underlying_error`. For `HookFailed(command, status, output)`, include a detail like `command <command> exited <status>: <output>`. For `HookTimedOut(command)`, include `command <command> timed out`. For `HookIo(message)`, include the message.
- `error.ProbeFailed(pi_error)` uses `error.pi_rpc_code(pi_error)` as `underlying_error` and the pi detail rules below.
- `error.PiFailed(pi_error)` uses `error.pi_rpc_code(pi_error)` as `underlying_error` and the pi detail rules below.
- `error.StateRefreshFailed(tracker_error)` uses `error.tracker_code(tracker_error)` as `underlying_error` and includes tracker detail when present.
- `error.OperatorAbort` and `error.OperatorStopAfterCurrentTurn` have no nested error; they may include a short detail such as `operator requested abort` or `operator requested stop after current turn`.

The pi detail rules should include enough information to distinguish failures:

- `PiLaunchFailed(message)`: detail `launch failed: <message>`.
- `PiMalformedJson(line)`: detail `pi emitted malformed JSON: <truncated line>`.
- `PiReadTimeout`: detail `timed out waiting for pi RPC response`.
- `PiTurnTimeout`: detail `pi turn timeout elapsed before agent_end`.
- `PiStallTimeout`: detail `pi stall timeout elapsed without output`.
- `PiExited(status)`: detail `pi process exited with status <status>`.
- `PiProtocolError(message)`: detail `pi protocol error: <message>`.

Truncate every detail string to a fixed constant before final redaction. Use a constant in `src/scherzo/handoff_format.gleam`, for example `const max_failure_detail_chars = 500`. If a detail is longer than the limit, append the same style of suffix used elsewhere, for example `… [truncated]`. If implementing this suffix would require awkward reuse from `runner.gleam`, define a local constant instead of exporting an unrelated runner constant.

After constructing the full body, call `log.redact("failure_comment", body, secrets)` and return the redacted body. `src/scherzo/handoff.gleam` should pass `tracker_secrets(tracker_config)` just as success comments do.

In `src/scherzo/handoff.gleam`, replace the inline string inside `report_failure` with:

    handoff_format.failure_comment(
      issue,
      failure,
      run_id,
      tracker_secrets(tracker_config),
    )

Keep the surrounding `run_comment`, `try_tracker`, and `run_state_update` flow unchanged.

In `test/handoff_test.gleam`, update the existing failure part of `comments_only_and_state_handoff_builds_expected_mutations_test`. Use a failure reason whose detail contains the configured secret, for example `error.PiFailed(error.PiProtocolError("secret-key blocked UI request"))`. Assert that the generated request body contains `Failure diagnostics`, `agent_pi_failed`, `pi_protocol_error`, and `blocked UI request`, and assert that it does not contain `secret-key`.

Add focused tests in `test/handoff_test.gleam` for at least three representative failure families. One test should cover a pi exit status with `error.PiFailed(error.PiExited(2))` and assert `pi_exited` and `status 2`. One test should cover a hook failure with `error.HookFailedError(error.HookFailed("scripts/jj-workspace-after-create", 17, "hook output"))` and assert `agent_hook_failed`, `hook_failed`, the command, the status, and the output. One test should cover workspace path handling by passing `workspace_path: Some("test/tmp/workflow-run/workspaces/implementation/ABC-123")` and asserting that relative path appears, then constructing a slash-prefixed synthetic workspace value from non-sensitive segments such as `"/" <> "operator-home/redacted-workspace"` in a separate assertion or test and asserting that the sentinel segment `operator-home` does not appear.

## Concrete Steps

1. From the repository root, run the focused handoff test file before editing to capture the baseline:

       direnv exec . gleam test test/handoff_test.gleam

   Expect the current tests to pass. If `direnv` reports that `.envrc` is blocked, inspect `.envrc`, run `direnv allow .`, and retry. If this exact single-file invocation is not supported by the current Gleam test setup, run `direnv exec . gleam test` and use the handoff test names in the output as the baseline.

2. Open `src/scherzo/handoff_format.gleam` and add any imports needed by the new formatter. It already imports `gleam/int`, `gleam/option`, `scherzo/agent/runner`, `scherzo/domain`, and `scherzo/log`; add `gleam/string` and `scherzo/error` if they are not already present.

3. In `src/scherzo/handoff_format.gleam`, add `const max_failure_detail_chars = 500` near the existing top-level definitions. If a suffix constant is useful, add `const failure_detail_truncated_suffix = "… [truncated]"`.

4. In `src/scherzo/handoff_format.gleam`, add `pub fn failure_comment(issue, failure, run_id, secrets)` below `success_result_attachment_markdown` or below `success_comment`. Build the header and the `Failure diagnostics` body, then redact the complete body before returning it.

5. In `src/scherzo/handoff_format.gleam`, add private helpers to compute the top-level code, nested code, detail, workspace line, and tokens line. Pattern match exhaustively on `error.AgentRunnerError`, `error.PiRpcError`, `error.HookError`, `error.WorkspaceError`, `error.TemplateError`, and `error.TrackerError` as needed.

6. Run formatting and the focused tests:

       direnv exec . gleam format --check src test
       direnv exec . gleam test test/handoff_test.gleam

   At this point the test command may still pass only existing tests if new tests have not been added yet. Fix compiler errors before continuing.

7. In `src/scherzo/handoff.gleam`, replace the inline failure comment string in `report_failure` with a call to `handoff_format.failure_comment(issue, failure, run_id, tracker_secrets(tracker_config))`. Do not change the claim, success, or state-update behavior.

8. In `test/handoff_test.gleam`, update `comments_only_and_state_handoff_builds_expected_mutations_test` so the failure assertion checks the richer diagnostics and redaction behavior. The fake transport can remain unchanged because it already captures the GraphQL request body.

9. In `test/handoff_test.gleam`, add a test named `failure_handoff_includes_nested_pi_diagnostics_test`. Construct a `runner.WorkerFailure` with `reason: error.PiFailed(error.PiExited(2))`, `workspace_path: None`, zero tokens, and no final issue. Call `client.report_failure(issue(), failure, "run-pi-exit")`. Assert the request body contains `Failure diagnostics`, `agent_pi_failed`, `pi_exited`, and `status 2`.

10. In `test/handoff_test.gleam`, add a test named `failure_handoff_includes_hook_diagnostics_test`. Construct a `runner.WorkerFailure` with `reason: error.HookFailedError(error.HookFailed("scripts/jj-workspace-after-create", 17, "hook output"))`. Assert the request body contains `agent_hook_failed`, `hook_failed`, `scripts/jj-workspace-after-create`, `17`, and `hook output`.

11. In `test/handoff_test.gleam`, add a test named `failure_handoff_handles_workspace_path_safely_test`. First use `workspace_path: Some("test/tmp/workflow-run/workspaces/implementation/ABC-123")` and assert that path appears. Then construct a slash-prefixed synthetic value from non-sensitive string segments, for example `let absolute_workspace = "/" <> "operator-home/redacted-workspace"`, use `workspace_path: Some(absolute_workspace)`, and assert the generated body does not contain `operator-home` and either omits `workspace:` or contains the planned safe placeholder.

12. Run the focused tests again:

       direnv exec . gleam test test/handoff_test.gleam

   Expect all handoff tests to pass. If the single-file command is not supported, run `direnv exec . gleam test` instead.

13. Run the full local validation:

       direnv exec . gleam format --check src test
       direnv exec . gleam test

   Expect formatting to pass and the full test suite to pass.

14. Commit after the tree is green. A suitable commit message is `Improve failure handoff diagnostics`.

## Testing and Falsifiability

The core claim is that Linear failure comments become more actionable without leaking configured secrets or becoming unbounded. The tests must prove this claim at the comment text boundary, not by inspecting private helpers.

Update `test/handoff_test.gleam` so the existing comments-only handoff test asserts the new failure body contains the old top-level signal and the new diagnostic section. The test should fail before implementation because the current body does not contain `Failure diagnostics` or the nested error code `pi_protocol_error`. It should pass after `handoff.gleam` calls the new formatter.

Add `failure_handoff_includes_nested_pi_diagnostics_test` with input `error.PiFailed(error.PiExited(2))`. The expected assertion is that the generated request body contains `agent_pi_failed`, `pi_exited`, and `status 2`. This falsifies the plan if pi failures still show only `agent_pi_failed`.

Add `failure_handoff_redacts_known_secret_details_test`, or fold the same assertion into the existing comments-only test. Use a detail string containing `secret-key`, because `tracker_config()` in `test/handoff_test.gleam` sets `api_key: Some("secret-key")`. Assert that `secret-key` does not appear and `[REDACTED]` or the non-secret surrounding text does appear. This falsifies the plan if the formatter bypasses `log.redact`.

Add `failure_handoff_truncates_long_details_test`. Use `error.PiFailed(error.PiProtocolError(long_message))` where `long_message` is longer than `max_failure_detail_chars`, such as 800 repeated characters plus a sentinel suffix `SHOULD_NOT_APPEAR`. Assert that `SHOULD_NOT_APPEAR` does not appear and `truncated` does appear. If Gleam makes repeated strings awkward in this test file, use a manually long literal short enough to keep the test readable but longer than the formatter limit.

Add `failure_handoff_includes_hook_diagnostics_test` for `error.HookFailedError(error.HookFailed("scripts/jj-workspace-after-create", 17, "hook output"))`. Assert top-level code, nested code, command, status, and short output. This falsifies the plan if only pi failures receive useful diagnostics.

Add `failure_handoff_handles_workspace_path_safely_test` for relative and absolute workspace paths. The relative-path case should show the path; the slash-prefixed synthetic case must not show the sentinel segment `operator-home`. This falsifies the plan if local host path segments can be posted to Linear.

Run:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Expected success is no format diff and a passing test suite. The exact number of tests may change as the repository evolves, so do not hard-code a pass count in implementation notes. Treat any failure in existing handoff tests as a regression unless the assertion was intentionally updated to match the richer failure comment.

## Validation and Acceptance

Implementation is accepted when a failed handoff comment generated by `test/handoff_test.gleam` has this observable shape in the captured GraphQL request body:

    Scherzo failed run run-3 for ABC-1.

    Failure diagnostics:
    - error: agent_pi_failed
    - underlying_error: pi_protocol_error
    - detail: pi protocol error: [REDACTED] blocked UI request
    - tokens: input=0 output=0 cache_read=0 cache_write=0 total=0

The exact JSON escaping in the GraphQL request body may differ, but the meaningful text must be present. The old substring `Scherzo failed run` must still be present. The top-level code must still be present. The nested code and useful detail must now be present. The configured secret `secret-key` must not be present.

Full acceptance requires:

- `src/scherzo/handoff.gleam` still posts a failure comment and still runs the configured failure state update afterward.
- `src/scherzo/handoff_format.gleam` owns the failure comment Markdown and redacts the complete body.
- `test/handoff_test.gleam` covers pi, hook, redaction, truncation, and workspace path behavior.
- `direnv exec . gleam format --check src test` passes.
- `direnv exec . gleam test` passes.

## Rollout, Recovery, and Idempotence

The change is additive at the Linear comment level. It does not alter how Scherzo decides that a workflow failed, whether it retries, how it cleans up workspaces, or which issue state it moves to. Existing operators who search for `Scherzo failed run` or the top-level error code will still find those strings.

If the richer comment proves too noisy or risky, rollback is straightforward: revert the commit that introduced `handoff_format.failure_comment` and the `report_failure` call site change. Because no data migration or persistent schema change is involved, rollback does not require cleanup.

If a failure happens while posting the comment, existing `run_comment` and `run_state_update` error handling remains responsible. Do not add a secondary failure path in this work.

The implementation steps are idempotent in the normal source-control sense: rerunning tests and formatting is safe. Re-running Scherzo itself may post another Linear comment for another failed run, but the code change does not add any new repeated side effect beyond the existing failure comment behavior.

## Artifacts and Notes

Current failure string, as observed in `src/scherzo/handoff.gleam`, is equivalent to:

    Scherzo failed run <run_id> for <issue.identifier> with error <agent_code>.

Target failure string shape:

    Scherzo failed run <run_id> for <issue.identifier>.

    Failure diagnostics:
    - error: <agent_code>
    - underlying_error: <nested_code>
    - detail: <redacted, truncated detail>
    - workspace: <safe relative workspace path when available>
    - tokens: input=<n> output=<n> cache_read=<n> cache_write=<n> total=<n>

The existing success metadata line in `src/scherzo/handoff_format.gleam` formats token totals as:

    tokens: input=1 output=2 cache_read=0 cache_write=0 total=3

Reuse that style for failure token totals so comments remain consistent.

## Interfaces and Dependencies

No new package dependencies are required.

At the end of the work, `src/scherzo/handoff_format.gleam` must expose this function:

    pub fn failure_comment(
      issue: domain.Issue,
      failure: runner.WorkerFailure,
      run_id: String,
      secrets: List(String),
    ) -> String

At the end of the work, `src/scherzo/handoff.gleam` must use that function from `report_failure` and must continue to pass `tracker_secrets(tracker_config)` as the redaction source.

The implementation should continue to use these existing types and functions:

- `runner.WorkerFailure` from `src/scherzo/agent/runner.gleam`.
- `domain.Issue` and `domain.TokenTotals` from `src/scherzo/domain.gleam`.
- `error.agent_code`, `error.pi_rpc_code`, `error.hook_code`, `error.workspace_code`, `error.template_code`, and `error.tracker_code` from `src/scherzo/error.gleam`.
- `log.redact` from `src/scherzo/log.gleam`.
- `handoff.tracker_secrets` indirectly through the existing private helper in `src/scherzo/handoff.gleam`; do not make it public unless the compiler or tests require it.

## Open Questions and Clarifications Needed

- [CLARIFY] Confirm whether failure comments may include arbitrary nested error detail after configured-secret redaction, or whether some error families should publish only codes and send users to daemon logs for raw detail.
