# Add Scherzo-to-Linear session result comments

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, Scherzo will post useful task results back to the Linear issue when a pi worker succeeds, not only operational handoff metadata. The observable behavior is that a successful run with `handoff.enabled: true` and `handoff.comment_on_success: true` creates one structured Linear success comment containing the Scherzo run ID, the run classification, token and turn metadata, and the visible assistant result captured from pi. The comment is written by Scherzo, not by pi, and Scherzo redacts configured secrets, truncates oversized output deterministically, and excludes tool calls, raw JSON, lifecycle events, and hidden thinking.

This plan covers only the Scherzo-to-Linear route. It does not define how Linear comments, labels, issue edits, or state changes can command Scherzo. That later Linear-to-Scherzo command transport should reuse the same command model as `scherzoctl` and is intentionally out of scope here.

## Problem Framing and Constraints

Current Scherzo handoff comments tell humans that a run was claimed, completed, or failed, but the success comment does not contain the work result. A human reading the Linear issue sees a line such as “Scherzo completed run ... with classification active and 7767 total pi tokens,” but must inspect daemon logs, a workspace, or future session attach tooling to know what the agent concluded. That is not enough for Linear to be the task collaboration surface.

The tempting rule is “post all model output except thinking and tool calls to Linear.” That is simple, but it can make issue threads noisy if every streamed delta or every turn is posted as a separate comment. This plan chooses a bounded version of that rule: Scherzo captures visible assistant output from each successful worker turn as a result artifact, then posts exactly one structured success comment for the worker run. The artifact is not a raw transcript. It includes assistant-visible text only; it excludes tool call payloads, raw pi JSON, UI request payloads, lifecycle events, and diagnostics. The result text is redacted and truncated before it can reach Linear.

The implementation must fit the current codebase. `src/scherzo/agent/runner.gleam` does not currently drive turns through the older one-shot `pi_rpc.prompt` helper. It sends the prompt with `pi_rpc.send_prompt`, emits any records returned as `skipped` while waiting for the prompt response, then streams records one at a time through `active_turn_loop` by repeatedly calling `pi_rpc.read_turn_record`. `src/scherzo/agent/pi_rpc.gleam` already decodes `message_update.delta`, tool records, UI request metadata, token totals, and raw JSON through `runner.PiUpdate`, but it does not decode final assistant messages from `agent_end.messages`. `runner.WorkerSuccess` currently carries `final_issue`, `final_classification`, `workspace_path`, `tokens`, and `turns`, but no result text. `src/scherzo/handoff.gleam` currently formats success comments inline and posts them through Linear `commentCreate`; it has no pure result-comment formatter and no result-specific config.

The result comment must remain safe and audit-friendly. Handoff remains disabled by default. When handoff is enabled, Scherzo remains the only component that writes Linear comments. pi may provide assistant text, but Scherzo owns the formatting, redaction, truncation, and posting. Linear issue description edits are deliberately deferred because append-only comments are easier to audit and roll back operationally.

## Strategy Overview

Add a small result-artifact data model and pure collector. The collector converts pi RPC records from one turn into visible assistant text. It prefers final assistant message content from `agent_end.messages` when present because that is more likely to represent the final assistant response for the turn. If `agent_end.messages` is absent or empty, it falls back to concatenating `message_update.delta` values in event order. It ignores all other event types. The collector redacts configured secrets using existing `log.redact`, appends turn results across in-worker continuation turns, and applies a configured maximum character count with a `truncated` flag.

Extend `runner.WorkerSuccess` with a `result` artifact. Because the current runner consumes pi output through `send_prompt` plus `active_turn_loop`, add an explicit per-turn record accumulator to the active loop instead of relying on `pi_rpc.prompt`'s returned event list. The accumulator must include records read by `read_turn_record` and records returned as `skipped` by command-response helpers such as `send_prompt`, `send_extension_ui_cancel`, and `send_extension_ui_value` when those records are part of the active turn. After `agent_end` closes a successful turn, the runner converts the accumulated records into a turn artifact and appends it to the worker-level result. The streaming `emit_update` path remains unchanged so EventHub and future attach surfaces continue to see live events without waiting for worker completion.

Extend handoff success comment formatting so the existing success comment becomes the result comment. This avoids adding a second comment for every successful run. The formatted comment includes run ID, issue identifier, classification, turns, token totals, and a `Result` section. If no assistant text was captured, the comment says so explicitly. If the artifact was truncated, the comment says so explicitly. Existing claim and failure comments remain operational comments.

Add minimal workflow configuration under `handoff:` to control result inclusion and size. The new fields are additive:

    handoff:
      include_result_on_success: true
      result_max_chars: 8000

`include_result_on_success` defaults to `comment_on_success`, so existing users who enable success comments get useful result text without adding another knob. `result_max_chars` defaults to `8000` and must be positive. If an operator sets `comment_on_success: false`, no success or result comment is posted.

## Alternatives Considered

One alternative is to post every assistant `message_update` to Linear as it streams. That is rejected for the first version because it would turn the issue thread into a terminal transcript, create many comments, and make duplicate/retry behavior harder to reason about.

Another alternative is to post two comments on success: one operational completion comment and one task result comment. That keeps metadata and content separate, but it doubles issue-thread noise and makes handoff idempotence harder because Scherzo does not yet persist Linear comment IDs. This plan combines metadata and result content into the existing success comment.

Another alternative is to have pi write directly to Linear. That is rejected. Scherzo owns Linear credentials, run IDs, redaction policy, truncation, and handoff state. pi output is untrusted input to Scherzo’s result formatter.

Another alternative is to edit the Linear issue description with the final answer. That is intentionally deferred. Description edits are harder to audit, can conflict with human edits, and require a separate conflict and rollback design.

Another alternative is to gather changed files or validation commands from the workspace in this phase. That would be useful, but Scherzo does not yet have a reliable, repository-agnostic changed-file inventory. Adding git diff parsing would expand the scope beyond the missing result-comment path. This phase leaves changed files and validation details for a future artifact plan.

## Risks and Countermeasures

The main usability risk is noisy Linear comments. Countermeasure: post one success/result comment per worker success, not one comment per event. Capture assistant-visible text only, format it under a clear `Result` heading, and truncate it deterministically.

The main correctness risk is capturing the wrong text or no text at all. `message_update.delta` can be incremental, while `agent_end.messages` may contain a final message snapshot. In the current runner, records also arrive through more than one path: ordinary active-turn reads and `skipped` records returned while waiting for command responses. Countermeasure: extend `pi_rpc.RpcRecord` to decode assistant message content from `agent_end.messages`, prefer the last non-empty assistant message from `agent_end` when present, and fall back to concatenated deltas only when no final assistant message is available. Thread a turn-record accumulator through `active_turn_loop` so records read by `read_turn_record` and relevant skipped records are both captured exactly once. Tests cover final-message preference, delta fallback, active-loop capture, and skipped/interleaved capture.

The main secrecy risk is posting secrets that appeared in assistant text. Countermeasure: the result collector receives `config.resolved_secrets(config)` and redacts configured secret values before storing the artifact in `WorkerSuccess`. The collector must redact both final-message content and delta-fallback content before truncation so a secret cannot survive because one source path skipped redaction. The handoff formatter also applies a final redaction pass using the tracker API key it has in scope before creating the Linear mutation request. Tests inject a fake secret in assistant output for both final-message and delta-fallback capture and assert the comment body does not contain it.

The main audit/idempotence risk is duplicate success comments. Countermeasure: this phase does not add retries for Linear mutation failures. The daemon already removes the worker handle before processing success and ignores stale `WorkerFinished` messages for a missing or mismatched run. Add a daemon or handoff test showing a single success report produces exactly one comment body containing the run ID and result text. Durable comment-id de-duplication across daemon crashes is deferred.

The main compatibility risk is changing `runner.WorkerSuccess` constructors across tests. Countermeasure: add the new field in one milestone, update all direct constructors in tests immediately, and keep the rest of the success/failure scheduling behavior unchanged.

The main scope risk is drifting into Linear-to-Scherzo command parsing. Countermeasure: this plan does not poll Linear comments, parse `/scherzo ...`, process labels, wake parked issues from comments, or edit descriptions. It only changes Scherzo-authored success comments.

## Progress

- [x] (2026-04-28 23:05Z) Read `docs/TODO.md` and confirmed the combined Linear communication TODO contains two separable routes: Scherzo-to-Linear result reporting and Linear-to-Scherzo command transport.
- [x] (2026-04-28 23:05Z) Read current `src/scherzo/agent/runner.gleam`, `src/scherzo/agent/pi_rpc.gleam`, `src/scherzo/handoff.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/domain.gleam`, `src/scherzo/config.gleam`, `src/scherzo/linear.gleam`, `src/scherzo/session/event.gleam`, and related tests before writing this plan.
- [x] (2026-04-28 23:05Z) Ran `direnv exec . gleam test`; the then-current baseline reported `116 passed, no failures`.
- [x] (2026-04-29) Re-ran `direnv exec . gleam test` during adversarial plan review; the current baseline reports `291 passed, no failures`.
- [x] (2026-04-29 22:55Z) Re-ran `direnv exec . gleam test` before implementation; the baseline still reports `291 passed, no failures`.
- [x] (2026-04-29) Reviewed the current runner integration and revised this plan away from the stale `pi_rpc.prompt` event-list assumption toward `send_prompt` plus `active_turn_loop` record accumulation.
- [x] (2026-04-29 23:08Z) Added result artifact domain/config fields, pure collector tests in `test/result_artifact_test.gleam`, and pure formatter tests in `test/handoff_format_test.gleam`.
- [x] (2026-04-29 23:08Z) Decoded final assistant messages from pi `agent_end.messages` into `RpcRecord.assistant_messages` and kept message deltas as fallback.
- [x] (2026-04-29 23:08Z) Carried redacted/truncated result artifacts through `runner.WorkerSuccess`, including active-loop records, prompt-response skipped records, and multi-turn accumulation.
- [x] (2026-04-29 23:08Z) Rendered structured success/result Linear comments through `handoff_format.success_comment` and `handoff.report_success`.
- [x] (2026-04-29 23:08Z) Updated `README.md`, `examples/WORKFLOW.md`, and this plan’s retrospective after validation.

## Surprises & Discoveries

- Observation: The current source tree already contains the EventHub and enriched `runner.PiUpdate` fields that older pending plans describe as future work.
  Evidence: `src/scherzo/orchestrator/daemon.gleam` starts an EventHub, `src/scherzo/session/` contains event/json/redaction/hub modules, and `runner.PiUpdate` already carries redacted raw JSON, turn, request id, method, pi session id, tokens, and tool name.

- Observation: The fake pi fixture already emits `agent_end` with `messages:[{role:"assistant",content:"done"}]`, but `src/scherzo/agent/pi_rpc.gleam` does not decode that field.
  Evidence: `test/fixtures/fake_pi_rpc.sh` emits the field, while `RpcRecord` currently contains `delta` but no assistant-message list.

- Observation: Current success comments are operational metadata only.
  Evidence: `src/scherzo/handoff.gleam` formats success as `Scherzo completed run ... with classification ... and ... total pi tokens.`

- Observation: The runner no longer uses the one-shot `pi_rpc.prompt` helper for worker turns; it sends a prompt, emits any skipped records from the command response, and then reads active-turn records through `active_turn_loop`.
  Evidence: `src/scherzo/agent/runner.gleam` calls `pi_rpc.send_prompt`, `emit_records` for `skipped`, `active_turn_loop`, and `pi_rpc.read_turn_record`; `active_turn_loop` currently returns only `ActiveTurn(session, prompt_queue, stop_after_turn)` and does not return the records it consumed.

- Observation: The plan's original baseline test count was stale.
  Evidence: `direnv exec . gleam test` on 2026-04-29 reports `291 passed, no failures`.

## Decision Log

- Decision: Combine task result content with the existing success handoff comment rather than posting a second result comment.
  Rationale: One comment per successful worker run keeps Linear issue threads quieter and preserves the existing run-ID audit trail.
  Date: 2026-04-28

- Decision: Capture assistant-visible output as a `WorkerSuccess` result artifact, not by querying EventHub during handoff.
  Rationale: The runner is already the component that sees ordered pi records for each successful turn. Carrying the artifact in `WorkerSuccess` keeps handoff deterministic and avoids making Linear posting depend on in-memory EventHub retention.
  Date: 2026-04-28

- Decision: Prefer final assistant content from `agent_end.messages` over streamed deltas when present.
  Rationale: Deltas are useful fallback and live UI data, but a final message snapshot is more likely to represent the intended task result and avoids duplicate incremental text.
  Date: 2026-04-28

- Decision: Capture result records inside the current `send_prompt`/`active_turn_loop` runner flow rather than reintroducing the older one-shot `pi_rpc.prompt` path.
  Rationale: The current runner needs operator commands and UI-request handling while a turn is active. Reverting to `pi_rpc.prompt` would bypass that control path and would be a larger, riskier refactor than the result-comment feature requires.
  Date: 2026-04-29

- Decision: Treat command-response `skipped` records as first-class turn records for result capture when they occur during a worker turn.
  Rationale: `pi_rpc.read_until_response_collect` can return records that were emitted before the matching command response. The runner already emits those records to EventHub, so omitting them from result capture would create a visible mismatch between streamed updates and the final Linear result.
  Date: 2026-04-29

- Decision: Defer changed-file and validation-command summaries.
  Rationale: Scherzo does not yet have a repository-agnostic changed-file or validation artifact. The immediate missing value is the assistant result text.
  Date: 2026-04-28

- Decision: Keep Linear issue description edits out of this phase.
  Rationale: Append-only comments are audit-friendly. Description edits require conflict handling and should have a separate explicit design.
  Date: 2026-04-28

## Outcomes & Retrospective

Implementation completed on 2026-04-29. Scherzo now builds a `ResultArtifact` from assistant-visible pi records, stores it on `runner.WorkerSuccess`, and formats the existing Linear success handoff comment as one structured result comment with run metadata, token totals, turns, redaction, and truncation. Deterministic fake-pi and fake-Linear tests cover final `agent_end.messages` preference, delta fallback, ignored tool/lifecycle events, secret redaction, truncation, skipped prompt-response records, multi-turn accumulation, formatter output, and single-comment handoff behavior.

Final validation from the repository root passed: `direnv exec . gleam format --check src test` exited zero, `direnv exec . gleam test` reported `309 passed, no failures`, and `direnv exec . gleam run -- --help` printed the CLI help successfully. Credential-gated real Linear validation was not run in this implementation session. Follow-up opportunities remain unchanged: changed-file summaries, validation-command summaries, Linear issue description edits, and durable Linear comment de-duplication should each get separate plans before implementation.

## Context and Orientation

Scherzo is a Gleam Erlang-target service. Source code lives under `src/scherzo/`; tests live under `test/`; validation is run from the repository root with `direnv exec . gleam test`.

The pi runner is `src/scherzo/agent/runner.gleam`. Its public `run_attempt` function prepares an issue workspace, renders the issue prompt, optionally probes pi, launches pi through `src/scherzo/agent/pi_rpc.gleam`, processes one or more turns, fetches session token stats, refreshes the issue state from Linear, and returns `Result(WorkerSuccess, WorkerFailure)`. `WorkerSuccess` now contains the final issue snapshot, final classification, workspace path, token totals, turn count, and a redacted/truncated `ResultArtifact`. The current worker turn flow is stepwise: `loop_turns` sends the prompt with `pi_rpc.send_prompt`, emits any skipped records returned before the prompt response, then calls `active_turn_loop`; `active_turn_loop` repeatedly calls `pi_rpc.read_turn_record`, handles operator commands and extension UI requests, emits each record as a `PiUpdate`, accumulates records for result collection, and stops when it sees `agent_end`.

The pi RPC client is `src/scherzo/agent/pi_rpc.gleam`. It sends JSON Lines commands to pi and decodes JSON Lines records into `RpcRecord`. `RpcRecord` now contains event type, optional command id, command name, success flag, session id, `delta`, UI method, token totals, tool metadata, decoded `assistant_messages` from top-level `agent_end.messages`, and raw JSON. The older `prompt` helper still exists and returns a full ordered event list after `agent_end`, but the runner path relevant to this plan uses `send_prompt`, `read_turn_record`, and command helpers that can return `skipped` records collected while waiting for command responses.

The Linear handoff module is `src/scherzo/handoff.gleam`. It builds a `handoff.Client` with `claim_issue`, `report_success`, and `report_failure`. When enabled, it posts comments through `linear.build_comment_create_request` and optionally moves issues by state ID through `linear.build_issue_update_state_request`. The daemon enqueues `ReportSuccess` after a worker succeeds; the side-effect runner calls `client.report_success(issue, success, run_id)`.

The EventHub under `src/scherzo/session/` records live session events, but this plan does not depend on EventHub for result comments. Result comments must still work if event retention is short, if the hub has dropped old events, or if a future deployment disables attach-like surfaces.

A result artifact in this plan means Scherzo’s bounded, redacted representation of visible assistant output from a successful worker. It is not raw pi JSON, not a full transcript, not tool output, not hidden thinking, and not a durable audit database.

## Preconditions and Verified Facts

The current baseline commands from the repository root are:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -- --help

On 2026-04-29 during adversarial review, `direnv exec . gleam test` ended with `291 passed, no failures`. The earlier `116 passed, no failures` note in this plan was from an older tree and is no longer the current baseline. After implementation on 2026-04-29, final validation ended with `309 passed, no failures`.

Current repository facts after implementation:

- `src/scherzo/agent/runner.gleam` defines `WorkerSuccess(final_issue, final_classification, workspace_path, tokens, turns, result)`.
- `src/scherzo/agent/runner.gleam` uses `pi_rpc.send_prompt`, emits `skipped` records from command responses, reads turn records with `pi_rpc.read_turn_record` inside `active_turn_loop`, and retains consumed active-turn records for result collection on successful turns.
- `src/scherzo/agent/runner.gleam` defines `ActiveTurn(session, prompt_queue, stop_after_turn, records)` and `ActiveCommandState(session, prompt_queue, stop_after_turn, pending_ui, stall_deadline_ms, records)` so skipped UI-command records and ordinary turn records stay with the active turn.
- `src/scherzo/agent/pi_rpc.gleam` defines `RpcRecord` with `delta: Option(String)`, tool metadata, decoded `assistant_messages: List(String)`, and `raw_json: String`.
- `src/scherzo/agent/pi_rpc.gleam` still exposes the older `prompt` and `prompt_with_ui_policy` helpers, but worker result capture uses the current `send_prompt`/`active_turn_loop` path.
- `test/fixtures/fake_pi_rpc.sh` emits `message_update` with a `delta` and emits `agent_end` with `messages:[{role:"assistant",content:"done"}]` unless `FAKE_PI_NO_AGENT_END_MESSAGES=1` is set.
- `test/fixtures/fake_pi_rpc.sh` can emit interleaved `message_update` records before abort and extension UI response command responses via `FAKE_PI_INTERLEAVE_EVENT_BEFORE_COMMAND_RESPONSE`, and before a prompt response via `FAKE_PI_INTERLEAVE_EVENT_BEFORE_PROMPT_RESPONSE`.
- `src/scherzo/handoff.gleam` leaves claim and failure comments operational, and delegates success comment body creation to `src/scherzo/handoff_format.gleam`.
- `src/scherzo/config.gleam` resolves `handoff.enabled`, `comment_on_claim`, `comment_on_success`, `comment_on_failure`, `claim_state_id`, `success_state_id`, `failure_state_id`, `include_result_on_success`, and `result_max_chars`.
- `src/scherzo/log.gleam` exposes `redact(key, value, secrets)` and `truncate(value, max)`; `truncate` returns the first `max` characters plus `...` when truncation happens, so tests should assert the chosen exact truncation contract rather than assuming the returned string length is exactly `max`.
- `src/scherzo/linear.gleam` already exposes `build_comment_create_request` and `parse_mutation_response` for Linear comments.

If any of these facts differ when implementing, normalize the plan against the current tree before editing code. Do not add a new Linear transport or a second scheduler path.

## Scope Boundaries

In scope: result artifact type; result collection from pi RPC records; decoding assistant content from `agent_end.messages`; fallback to `message_update.delta`; redaction and truncation; adding the artifact to `runner.WorkerSuccess`; extending handoff config with result inclusion and maximum size; pure success comment rendering; replacing the existing success comment body with a structured metadata-plus-result body; deterministic tests for capture, formatting, redaction, truncation, config defaults, and handoff mutation bodies; README and example workflow documentation.

Out of scope: Linear-to-Scherzo commands; polling Linear comments; parsing `/scherzo` syntax; wake-up behavior; authorization by Linear user; issue description edits; changed-file detection; validation-command extraction; posting progress comments while a worker is still running; posting failure transcripts; durable de-duplication across daemon crashes; Linear comment update/delete behavior.

## Milestones

Milestone 1 adds the result artifact model and pure collector. At the end, tests can feed decoded pi records into a pure function and get redacted, truncated assistant-visible text without launching pi.

Milestone 2 extends pi record decoding. At the end, `pi_rpc.decode_record` extracts assistant message content from `agent_end.messages`, and collector tests prove final assistant messages are preferred over incremental deltas when present.

Milestone 3 carries result artifacts through the runner's current active-turn machinery. At the end, `runner.run_attempt` returns `WorkerSuccess.result` for fake pi, including records read through `active_turn_loop`, records returned as command-response `skipped` records during the turn, multi-turn accumulation, redaction, and truncation, while live `emit_update` behavior remains unchanged.

Milestone 4 formats and posts structured success/result comments. At the end, `handoff.report_success` creates one Linear comment body with run ID, classification, turns, tokens, and the result section, or the previous metadata-only shape when configured not to include the result.

Milestone 5 updates documentation and validates the whole phase. At the end, README and `examples/WORKFLOW.md` describe result comment behavior, tests pass, and the plan records final outcomes.

## Plan of Work

Add result artifact types to `src/scherzo/domain.gleam`. Define:

    pub type ResultArtifact {
      ResultArtifact(
        final_response: Option(String),
        truncated: Bool,
        source: String,
      )
    }

`source` should initially be either `agent_end_messages`, `message_update_delta`, `combined_turns`, or `none`. Keep it a string to avoid a public enum migration if future pi event names change.

Extend `domain.HandoffConfig` with:

    include_result_on_success: Bool
    result_max_chars: Int

Update `config.default_handoff_config` so `include_result_on_success` is `False` when handoff is disabled. In `resolve_handoff`, default `include_result_on_success` to `comment_on_success` after that field has been resolved. Default `result_max_chars` to `8000`; reject values less than or equal to zero with `error.InvalidConfig("handoff.result_max_chars must be positive")`.

Create `src/scherzo/result_artifact.gleam`. Expose pure helpers equivalent to:

    pub fn empty() -> domain.ResultArtifact

    pub fn from_records(
      records: List(pi_rpc.RpcRecord),
      secrets: List(String),
      max_chars: Int,
    ) -> domain.ResultArtifact

    pub fn append(
      existing: domain.ResultArtifact,
      next: domain.ResultArtifact,
      max_chars: Int,
    ) -> domain.ResultArtifact

`from_records` first finds non-empty assistant message content decoded from `agent_end.messages` into `RpcRecord.assistant_messages`. If present, use the last such content as the turn text with source `agent_end_messages`. If not present, concatenate `delta` values from records whose `type_ == "message_update"` in event order with source `message_update_delta`. Ignore all other records, including `message` tool-call/tool-result records, `tool_execution_*`, `agent_start`, `turn_start`, `turn_end`, `response`, and `extension_ui_request`. If no text exists, return `empty()`. Redact the chosen text with `log.redact("assistant_output", text, secrets)` before truncation. Use one deterministic truncation policy everywhere; either call `log.truncate(redacted, max_chars)` and set `truncated: True` when `string.length(redacted) > max_chars`, or implement an equivalent local helper and test the exact suffix behavior. `append` combines already-redacted non-empty artifacts across turns with two newlines between turn outputs, reapplies the same truncation cap, preserves `truncated: True` if either input was already truncated or the combined text is truncated, and sets source to `combined_turns` when both sides have text.

Modify `src/scherzo/agent/pi_rpc.gleam`. Add a field to `RpcRecord` such as `assistant_messages: List(String)`. Extend `record_decoder` so it decodes an optional top-level `messages` list. Each message with `role == "assistant"` and string `content` contributes to `assistant_messages`; other roles and non-string content are ignored. Existing tests and constructors must be updated for the new field. This decoder should be permissive: if `messages` is absent, use an empty list.

Modify `src/scherzo/agent/runner.gleam`. Add `result: domain.ResultArtifact` to `WorkerSuccess`. Add a worker-level result accumulator argument to `loop_turns`, initialized with `result_artifact.empty()`. Also add a per-turn record accumulator to the active-turn path. One concrete shape is to extend `ActiveTurn` to `ActiveTurn(session, prompt_queue, stop_after_turn, records)` and extend `ActiveCommandState` with `records`; then add a `turn_records` parameter to `active_turn_loop`, `handle_active_command`, `handle_turn_record`, `handle_extension_ui_record`, `handle_blocking_ui_policy`, `handle_ui_response_command`, and `handle_operator_ui_timeout` only where needed to preserve records across recursion and command handling.

In the successful `pi_rpc.send_prompt` branch, bind the returned list as `skipped`, emit it as today, and pass those skipped records as the initial per-turn record accumulator. The accumulator may be stored in reverse order for efficient prepends, but `ActiveTurn.records` must be returned in event order before calling the result collector. When `active_turn_loop` reads a record from `pi_rpc.read_turn_record`, add it to the per-turn accumulator at the same point it emits the update. When `send_extension_ui_cancel` or `send_extension_ui_value` returns skipped records during an active turn, emit those records as today and add them to the same per-turn accumulator before continuing the loop. When `agent_end` closes the turn, return the accumulated records in `ActiveTurn.records`. Then call `result_artifact.from_records(records, config_module.resolved_secrets(config), config.handoff.result_max_chars)`, append it to the worker-level result accumulator, and pass the updated artifact through `finish_after_turn`, `decide_after_refresh`, recursive `loop_turns`, and `finish_success`. `finish_success` stores the accumulated artifact in `WorkerSuccess.result`.

Keep the streaming update path unchanged. `update_from_record` should continue to emit redacted `message_update` deltas and raw JSON to EventHub. The result collector must use the explicit record accumulator, not the `emit_update` side effect, so it cannot double-count events. Do not refactor worker turns back to `pi_rpc.prompt`; that helper does not model the current operator-command and UI-response path.

Create `src/scherzo/handoff_format.gleam` or an equivalently named pure formatter module. The formatter should expose:

    pub fn success_comment(
      issue: domain.Issue,
      success: runner.WorkerSuccess,
      run_id: String,
      include_result: Bool,
      secrets: List(String),
    ) -> String

Format the comment as Markdown without relying on external state. Use this shape, with exact wording established by tests:

    Scherzo completed run <run_id> for <issue.identifier>.

    Result:
    <assistant result text, or _No assistant result text was captured._>

    Metadata:
    - classification: <active|terminal|non_active>
    - turns: <turns>
    - tokens: input=<input> output=<output> cache_read=<cache_read> cache_write=<cache_write> total=<total>

If `include_result` is false, omit the `Result:` section and keep the metadata. If `success.result.truncated` is true, append a short note after the result text such as `_Result truncated by Scherzo._`. Before returning the body, apply `log.redact("comment_body", body, secrets)`. The secrets list passed from handoff can contain the tracker API key; the result text was already redacted with the full effective config secret list inside the runner.

Modify `src/scherzo/handoff.gleam` to call the pure formatter from `report_success`. Pass `handoff_config.include_result_on_success` and a secrets list built from `tracker_config.api_key` when available. Leave claim and failure comments unchanged except for any constructor updates required by `HandoffConfig` or `WorkerSuccess`.

Update README and `examples/WORKFLOW.md` to document `handoff.include_result_on_success` and `handoff.result_max_chars`. Explain that Scherzo posts assistant-visible result text, not tool output or full session transcripts, and that issue description edits are not implemented.

## Concrete Steps

Keep the tree green at each checkpoint. Suggested commit points are after step 9 for config/domain constructor compatibility, after step 19 for pi decoding and the pure result artifact, after step 28 for runner integration, after step 37 for handoff formatting/posting, and after step 40 for documentation plus final validation. Use focused messages such as `Add result handoff config`, `Collect assistant result artifacts`, `Carry pi results through runner`, and `Format Linear success results`.

1. From the repository root, run `direnv exec . gleam test` and record the current pass count in this plan’s Progress section. The current expected output after the 2026-04-29 review is `291 passed, no failures`; if the count differs but there are no failures, record the new count before implementation.

2. In `src/scherzo/domain.gleam`, add `ResultArtifact` as specified above. Add `include_result_on_success: Bool` and `result_max_chars: Int` to `HandoffConfig`.

3. Update direct `domain.HandoffConfig` constructors in tests and source. Initially set `include_result_on_success: False` and `result_max_chars: 8000` where the test does not care about result content.

4. Update direct `runner.WorkerSuccess` constructors in tests by adding `result: domain.ResultArtifact(final_response: None, truncated: False, source: "none")`. This keeps the tree compiling before the collector exists.

5. Run `direnv exec . gleam test`. Fix constructor compile errors only; do not change behavior yet.

6. Add `test/config_test.gleam` coverage for `handoff_result_defaults_follow_success_comments_test`. Use workflow front matter with `handoff.enabled: true` and no explicit result settings. Assert resolved config has `comment_on_success == True`, `include_result_on_success == True`, and `result_max_chars == 8000`.

7. Add `test/config_test.gleam` coverage for `handoff_can_disable_result_in_success_comment_test`. Set `include_result_on_success: false` and assert success comments remain enabled while result inclusion is false.

8. Add `test/config_test.gleam` coverage for `handoff_result_max_chars_must_be_positive_test`. Set `result_max_chars: 0` and assert `config.resolve_with_env` returns `Error(error.InvalidConfig(_))`.

9. Update `src/scherzo/config.gleam` `default_handoff_config` and `resolve_handoff` until the config tests pass. Run `direnv exec . gleam test`.

10. Modify `src/scherzo/agent/pi_rpc.gleam` so `RpcRecord` includes `assistant_messages: List(String)`, defaulting to `[]`.

11. Add `test/pi_rpc_test.gleam` test `decode_agent_end_assistant_messages_test`. Decode this JSON:

        {"type":"agent_end","messages":[{"role":"assistant","content":"final answer"},{"role":"user","content":"ignored"}]}

    Assert `record.assistant_messages == ["final answer"]`.

12. Implement the `messages` decoder in `pi_rpc.gleam`. It should ignore missing `messages`, non-assistant roles, and non-string content. Run `direnv exec . gleam test`.

13. Create `src/scherzo/result_artifact.gleam` with `empty`, `from_records`, and `append`.

14. Create `test/result_artifact_test.gleam`. Add `prefers_agent_end_assistant_message_test`: construct records or decode JSON records for `message_update` with delta `draft` and `agent_end` with assistant content `final`; assert `from_records(...).final_response == Some("final")` and source is `agent_end_messages`.

15. In `test/result_artifact_test.gleam`, add `falls_back_to_message_update_deltas_test`: use two `message_update` records with `hello ` and `world`; assert final response is `hello world` and source is `message_update_delta`.

16. In `test/result_artifact_test.gleam`, add `ignores_tool_and_lifecycle_events_test`: include `tool_execution_start`, `tool_execution_update`, `turn_start`, and `agent_end` with no messages; assert no final response is captured.

17. In `test/result_artifact_test.gleam`, add `redacts_final_message_result_text_test`: include an `agent_end` assistant message containing `secret-key`, pass secrets `["secret-key"]`, and assert the final response contains `[REDACTED]` and does not contain `secret-key`.

18. In `test/result_artifact_test.gleam`, add `redacts_and_truncates_delta_fallback_text_test`: include `message_update.delta` text containing `secret-key`, omit assistant messages, pass secrets `["secret-key"]` and `max_chars: 20`, and assert the final response contains `[REDACTED]`, does not contain `secret-key`, and has `truncated == True` when the redacted text exceeds the cap.

19. Run `direnv exec . gleam test`; result artifact tests should pass.

20. Modify `src/scherzo/agent/runner.gleam` to thread a worker-level `domain.ResultArtifact` accumulator through `loop_turns`, `finish_after_turn`, `decide_after_refresh`, recursive `loop_turns`, and `finish_success`. Initialize it with `result_artifact.empty()` immediately after `pi_rpc.launch` succeeds.

21. In `src/scherzo/agent/runner.gleam`, extend the active-turn state so records consumed during a turn are returned to `loop_turns`. A concrete implementation is: add `records: List(pi_rpc.RpcRecord)` to `ActiveTurn`; add `records: List(pi_rpc.RpcRecord)` to `ActiveCommandState`; add a `turn_records` argument to `active_turn_loop` and to active command/record helpers that need to return updated records; pass `skipped` from `pi_rpc.send_prompt` as the initial value after emitting it; add each `read_turn_record` record as it is emitted; add skipped records from `send_extension_ui_cancel` and `send_extension_ui_value` before continuing; and return records in event order when `agent_end` ends the turn, reversing an internal prepend accumulator if necessary.

22. In `src/scherzo/agent/runner.gleam`, after `active_turn_loop` returns `Ok(ActiveTurn(session, prompt_queue, stop_after_turn, records))`, call `result_artifact.from_records(records, config_module.resolved_secrets(config), config.handoff.result_max_chars)`, append it to the worker-level accumulator with `result_artifact.append`, and pass the updated result into `finish_after_turn`. Do not use `pi_rpc.prompt` in this path.

23. Add `test/agent_runner_test.gleam` test `worker_success_includes_final_assistant_result_test`. Use the fake pi fixture as-is, run one terminal turn, and assert `success.result.final_response == Some("done")` because fake pi emits `agent_end.messages` with assistant content `done`. This test specifically proves records streamed through `active_turn_loop` are captured.

24. Add `test/agent_runner_test.gleam` test `worker_success_result_redacts_secret_output_test`. Use a fake pi mode that places a configured secret in visible assistant output. If the existing fixture only injects the secret into `message_update.delta`, add a fixture flag such as `FAKE_PI_NO_AGENT_END_MESSAGES=1` to force delta fallback. Assert `success.result.final_response` does not contain the secret and contains `[REDACTED]`.

25. Extend `test/fixtures/fake_pi_rpc.sh` only as much as needed for skipped-record capture. Add `FAKE_PI_INTERLEAVE_EVENT_BEFORE_PROMPT_RESPONSE=1` so the `prompt)` branch emits `jq -cn '{type:"message_update",delta:"interleaved"}'` before the prompt `response`. Add `FAKE_PI_NO_AGENT_END_MESSAGES=1` so the final `agent_end` is emitted without a `messages` field, forcing delta fallback for tests that need to observe skipped deltas.

26. Add `test/agent_runner_test.gleam` test `worker_success_result_includes_interleaved_skipped_records_test`. Run fake pi with `FAKE_PI_INTERLEAVE_EVENT_BEFORE_PROMPT_RESPONSE=1` and `FAKE_PI_NO_AGENT_END_MESSAGES=1`, run one successful terminal turn, and assert `success.result.final_response` contains `interleaved`. This test should fail before the active-loop record accumulator exists because the skipped record is emitted live but discarded before `WorkerSuccess` is built.

27. Add or update a multi-turn runner test so an active issue with `max_turns: 2` accumulates visible result text across turns and still returns `success.turns == 2`.

28. Run `direnv exec . gleam test`; runner tests should pass without changing existing EventHub update expectations.

29. Create `src/scherzo/handoff_format.gleam` with pure `success_comment` formatting and helper functions for classification and token formatting.

30. Add `test/handoff_format_test.gleam`. Add `success_comment_includes_result_and_metadata_test`: build a `WorkerSuccess` with result `Some("Implemented the fix.")`, tokens, and turns. Assert the comment contains the run ID, issue identifier, `Result:`, `Implemented the fix.`, classification, turns, and exact token metadata.

31. In `test/handoff_format_test.gleam`, add `success_comment_omits_result_when_disabled_test`: pass `include_result: False` and assert the body contains metadata but no `Result:` heading.

32. In `test/handoff_format_test.gleam`, add `success_comment_marks_truncated_result_test`: use `truncated: True` and assert the truncation note appears.

33. In `test/handoff_format_test.gleam`, add `success_comment_redacts_tracker_secret_test`: pass a result containing `secret-key` and secrets `["secret-key"]`; assert the returned comment does not contain `secret-key`.

34. Modify `src/scherzo/handoff.gleam` `report_success` to call `handoff_format.success_comment`. Pass `handoff_config.include_result_on_success` and a secrets list containing `tracker_config.api_key` if present.

35. Update `test/handoff_test.gleam`. Existing success assertions should now expect the structured comment body. Add an assertion that the success comment contains the result text when `include_result_on_success` is true and does not contain any injected secret.

36. Add a handoff test `success_handoff_posts_single_structured_result_comment_test` if not covered by existing tests. With a fake transport subject, call `client.report_success` once and assert exactly one `commentCreate` body is received for success when no success state update is configured.

37. Run `direnv exec . gleam test`; handoff tests should pass.

38. Update `README.md` under `Workflow schema` and `Linear handoff` to document `include_result_on_success`, `result_max_chars`, the one-comment success/result policy, and the fact that Scherzo excludes tools/raw JSON/thinking from result comments.

39. Update `examples/WORKFLOW.md` with commented result settings under `handoff:`. Keep handoff disabled by default.

40. Run final validation from the repository root:

        direnv exec . gleam format --check src test
        direnv exec . gleam test
        direnv exec . gleam run -- --help

    Expect format to exit zero, tests to pass, and help output to remain valid. Record the final test count in Progress.

41. Optional credential-gated validation: with a private Linear test project and fake pi, run one safe issue with `handoff.enabled: true`, `comment_on_success: true`, and `include_result_on_success: true`. Accept only if Linear receives one claim comment and one structured success/result comment containing the run ID and result text, with no API key or fake secret.

42. Update Outcomes & Retrospective with the final behavior, any skipped credential-gated validation, and follow-up items for changed-file summaries or Linear issue edits.

43. If the work has not already been split at the suggested checkpoints, commit the completed phase with a message such as `Post Scherzo session results to Linear` after the tree is green.

## Testing and Falsifiability

The result collector is falsified if it captures tool output, lifecycle events, UI payloads, command `response` records, or raw JSON as the final response; if it misses assistant text in `agent_end.messages`; if it fails to concatenate `message_update.delta` values when no final messages exist; if final-message and delta-fallback paths do not both redact configured secrets; or if truncation does not set `truncated: True`.

The runner integration is falsified if `WorkerSuccess.result` is empty for the existing fake pi success path, if records streamed through `active_turn_loop` are not captured, if command-response skipped records emitted during a turn are not captured, if multi-turn results overwrite earlier turn output without the defined two-newline append policy, if result collection breaks live `emit_update` streaming, or if failures start posting partial assistant output through the success path.

The handoff integration is falsified if enabled success handoff posts multiple success/result comments for one `report_success` call, if disabling `include_result_on_success` removes the success metadata comment entirely, if the comment omits the run ID, if a secret appears in the Linear mutation body, or if state-update behavior regresses.

Add deterministic tests in `test/pi_rpc_test.gleam`, `test/result_artifact_test.gleam`, `test/agent_runner_test.gleam`, `test/handoff_format_test.gleam`, `test/handoff_test.gleam`, and `test/config_test.gleam` as described in the Concrete Steps. No deterministic test may require real Linear credentials, real network access, or a real pi executable. Use `test/fixtures/fake_pi_rpc.sh` and fake Linear transports.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Before implementation, tests that reference `WorkerSuccess.result`, `RpcRecord.assistant_messages`, `ActiveTurn.records`, or `result_artifact` should fail to compile. After implementation, all tests should pass. The skipped-record runner test should fail before the active-loop accumulator exists because the only copy of the interleaved record is emitted live and then discarded before `WorkerSuccess` is built.

## Validation and Acceptance

Accept this phase when deterministic tests pass and the formatted success comment for a fake successful run has this behavior:

- It is posted by Scherzo through the existing Linear handoff client.
- It contains the Scherzo run ID and issue identifier.
- It contains a `Result:` section when `include_result_on_success` is true.
- It contains assistant-visible result text captured from pi records consumed by the current `send_prompt`/`active_turn_loop` path, including eligible skipped records returned by command-response helpers.
- It does not contain tool-call output, command response JSON, raw JSON, hidden thinking, configured secrets, or API keys.
- It includes classification, turns, and token totals.
- It marks truncated output when truncation happened.
- It remains one success comment per worker success.

Manual Linear validation is optional but recommended after deterministic tests. Use a private test issue, fake pi, and handoff comments only before enabling real pi.

## Rollout, Recovery, and Idempotence

This phase is additive and handoff remains disabled by default. Existing deployments with `handoff.enabled: false` continue to make no Linear writes. Deployments with `handoff.enabled: true` and `comment_on_success: true` will see richer success comments by default because `include_result_on_success` follows `comment_on_success`. Operators who want metadata-only comments can set `include_result_on_success: false`.

If Linear comment creation fails, current behavior remains: the daemon logs `handoff_success_failed` and does not rerun the worker. Because Scherzo does not persist Linear comment IDs, it cannot deduplicate comments across daemon crash/restart in this phase. The run ID in each comment remains the correlation key.

If result capture is wrong or too noisy, operators can disable only the result section while keeping operational success comments. The captured artifact lives only in the in-memory `WorkerSuccess` path and the posted Linear comment; there is no data migration.

## Artifacts and Notes

Representative target success comment:

    Scherzo completed run LIV-9-576460751521 for LIV-9.

    Result:
    Implemented the parser fix and updated the failing tests.

    Metadata:
    - classification: terminal
    - turns: 1
    - tokens: input=1200 output=300 cache_read=0 cache_write=0 total=1500

When no assistant output is captured:

    Result:
    _No assistant result text was captured._

When truncated:

    Result:
    <truncated text>

    _Result truncated by Scherzo._

Do not add changed files in this phase. A future artifact plan can add changed files after Scherzo has a safe repository-aware diff contract.

## Interfaces and Dependencies

In `src/scherzo/domain.gleam`, add:

    pub type ResultArtifact {
      ResultArtifact(
        final_response: Option(String),
        truncated: Bool,
        source: String,
      )
    }

Extend `HandoffConfig` to:

    pub type HandoffConfig {
      HandoffConfig(
        enabled: Bool,
        comment_on_claim: Bool,
        comment_on_success: Bool,
        comment_on_failure: Bool,
        claim_state_id: Option(String),
        success_state_id: Option(String),
        failure_state_id: Option(String),
        include_result_on_success: Bool,
        result_max_chars: Int,
      )
    }

In `src/scherzo/agent/pi_rpc.gleam`, extend `RpcRecord` to include:

    assistant_messages: List(String)

The field contains only decoded top-level `messages` entries whose `role` is `assistant` and whose `content` is a string.

In `src/scherzo/agent/runner.gleam`, extend `WorkerSuccess` to include:

    result: domain.ResultArtifact

Also extend the private active-turn state enough to return consumed records in event order to `loop_turns`. If using the concrete shape from this plan, `ActiveTurn` becomes:

    ActiveTurn(
      session: pi_rpc.Session,
      prompt_queue: List(String),
      stop_after_turn: Bool,
      records: List(pi_rpc.RpcRecord),
    )

and `ActiveCommandState` carries a matching `records: List(pi_rpc.RpcRecord)` field so skipped records from UI response commands are not lost.

In `src/scherzo/result_artifact.gleam`, expose:

    pub fn empty() -> domain.ResultArtifact

    pub fn from_records(
      records: List(pi_rpc.RpcRecord),
      secrets: List(String),
      max_chars: Int,
    ) -> domain.ResultArtifact

    pub fn append(
      existing: domain.ResultArtifact,
      next: domain.ResultArtifact,
      max_chars: Int,
    ) -> domain.ResultArtifact

In `src/scherzo/handoff_format.gleam`, expose:

    pub fn success_comment(
      issue: domain.Issue,
      success: runner.WorkerSuccess,
      run_id: String,
      include_result: Bool,
      secrets: List(String),
    ) -> String

No new package dependency should be required. Use existing `gleam_json`, `gleam/dynamic/decode`, `scherzo/log`, `scherzo/config`, and Linear mutation helpers.
