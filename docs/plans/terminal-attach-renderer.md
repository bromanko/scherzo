# Add a human-readable terminal attach renderer

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, an operator can attach to a Scherzo worker from a terminal and understand what the agent is doing without reading raw JSON or compact event lines. The visible proof is that `scherzoctl attach <session-id>` replays recent retained events and follows new ones, grouping output by turn, rendering assistant text as assistant text, rendering tool executions as tool blocks when tool fields are present, showing blocking UI requests distinctly, warning when retained history is truncated, and printing a concise token summary at turn end. `scherzoctl attach --raw <session-id>` continues to expose compact event lines for debugging, and `scherzoctl attach --json <session-id>` continues to expose one structured JSON object per event for automation.

This phase changes presentation only. It does not add mutating controls, does not answer UI requests, does not abort or retry workers, and does not change worker behavior.

## Problem Framing and Constraints

The local read-only control API makes session data available, but raw event streams are too low-level for everyday operation. Operators need to answer questions like: Is the worker thinking, editing, running a shell command, waiting on a UI request, compacting context, retrying inside pi, or done? Pi's default interactive rendering is good at this, but Scherzo currently stores and serves pi RPC events through its own `session.Event` model. Unless pi exposes a reusable renderer library in this repository, Scherzo needs its own terminal renderer that preserves the same spirit: grouped turns, clear role labels, tool boundaries, and useful progress without overwhelming noise.

The renderer must be robust to unknown pi event types, partial retained history, replay followed by live streaming, colorless terminals, narrow terminals, and automation users that depend on raw or JSON output. It must not change session state, worker behavior, control authentication, Linear interactions, or the EventHub retention policy.

`events <session-id>` is intentionally not changed to pretty output by default in this phase. It keeps the existing compact line output for compatibility with operators and lightweight scripts. Pretty replay is available as `events --pretty <session-id>`.

## Strategy Overview

First normalize the event data that the renderer needs. The current tree already has `src/scherzo/session/event.gleam`, `src/scherzo/session/json.gleam`, `src/scherzo/agent/pi_rpc.gleam`, `src/scherzo/agent/runner.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/control/client.gleam`, and `src/scherzo/ctl.gleam`. The renderer depends on those modules preserving enough structured fields to identify assistant messages, tool execution start/update/end events, UI requests, token totals, and redacted raw fallbacks. Tool and assistant classification must be fixed before the terminal renderer is wired into the CLI.

Then add a pure renderer module under `src/scherzo/terminal/`. Rendering state stays separate from I/O. The renderer returns display chunks rather than directly printing so tests can feed known event sequences and assert exact output. Output chunks distinguish complete lines from inline assistant text, which allows two `message_update` deltas such as `hello ` and `world` to appear as continuous `hello world` text during live attach.

Finally update `scherzoctl attach` to use the renderer by default and add `events --pretty` for non-following pretty replay. Keep raw and JSON contracts exact and stable: raw output uses the existing `client.compact_event_line` format, `events --json` keeps the current protocol response envelope, and `attach --json` prints one existing stream-event JSON envelope per event.

This approach is proportionate because it avoids a full-screen TUI, avoids embedding a second pi process, and avoids changing the control protocol except for additive optional event fields needed for human rendering.

## Alternatives Considered

One alternative is to pipe raw pi RPC events into a pi process and ask pi to render them. There is no confirmed stable interface for that in this repository, and it would make attachment depend on another live pi process.

Another alternative is to build a full-screen TUI immediately. That is attractive for navigation across many workers, but it adds stateful terminal complexity before the event taxonomy is proven. A streaming line-oriented renderer is simpler, works over SSH, and can be used by scripts.

A third alternative is to change `events <session-id>` to pretty output by default. That would make replay more readable, but it risks breaking existing habits and simple parsers that consume compact event lines. This plan keeps `events` compact by default and adds `events --pretty` instead.

A fourth alternative is to keep only JSON output and tell operators to use `jq`. That is insufficient for the stated operator goal: humans want to attach and see turns, tools, and messages in a readable form.

## Risks and Countermeasures

The main usability risk is noisy output that is technically rendered but not understandable. Countermeasure: group by turn, suppress redundant lifecycle events by default, render assistant deltas as continuous inline text where possible, and show tool start/end as clear blocks.

The main data-shape risk is that current Scherzo events do not normalize tool fields. Countermeasure: add additive optional fields to `EventPayload` before writing the renderer, update JSON encode/decode tests, and update fake pi fixtures to emit deterministic tool events. If real pi event payloads later use different field names, unknown events still render through the fallback path and the field aliases can be expanded without breaking the CLI contract.

The main compatibility risk is unknown or changing pi event types. Countermeasure: every event has a fallback renderer that prints `event <pi_type-or-name>` and, when `show_raw_unknown` is enabled, a compact redacted raw payload excerpt. Unknown events must never crash attach.

The main terminal risk is ANSI color causing unreadable logs or test instability. Countermeasure: support `--color=auto|always|never`; renderer tests use `ColorNever`; `attach --json`, `attach --raw`, `events --json`, and default `events` never emit ANSI. In this phase, `ColorAuto` is defined as no color unless a small terminal-detection helper is implemented and tested.

The main stream risk is duplicate output when replay transitions to live follow. Countermeasure: replay with `get_events(session_id, since_cursor, 200)`, render replayed events while updating `RenderState.last_cursor`, then follow with `stream_events(session_id, state.last_cursor)`. As a defensive guard, `render_event` ignores any event with `cursor <= state.last_cursor`.

The main retained-history risk is misleading output when old events have been dropped by EventHub retention. Countermeasure: if `EventPage.truncated` is `True`, pretty replay prints a visible warning before rendered events.

The main redaction risk is leaking secrets through newly normalized tool fields. Countermeasure: every decoded tool input/output/status string is passed through the same secret-redaction path used for assistant deltas before it becomes a `session.EventPayload`; raw JSON remains recursively redacted and capped as it is today.

## Progress

- [x] (2026-04-28 18:40Z) Confirmed this phase depends on the read-only control API and does not add mutating controls.
- [x] (2026-04-28 19:20Z) Revised the plan after adversarial review to keep `events` compact by default, normalize tool fields before rendering, define output chunk semantics, preserve exact raw/JSON contracts, and add replay/follow duplicate tests.
- [ ] Run `direnv exec . gleam test` and record the current pass count before implementation.
- [ ] Normalize assistant and tool event fields through pi RPC, runner, EventHub, session JSON, and control protocol tests.
- [ ] Add pure ANSI/style helpers and renderer output chunk/state types.
- [ ] Add renderer tests for turn grouping, assistant deltas, tool events, UI requests, unknown events, truncation warnings, duplicate cursors, and token summaries.
- [ ] Refactor `scherzoctl` command options and test seams.
- [ ] Update `scherzoctl attach` to use pretty rendering by default and add `events --pretty`.
- [ ] Update documentation with terminal attach examples.

## Surprises & Discoveries

(To be filled during implementation. In particular, record any real pi event payload fields that differ from the fake fixture, any missing tool/compaction/retry fields, and any compromises made for renderer output.)

## Decision Log

- Decision: Build a line-oriented renderer before a full-screen TUI.
  Rationale: It satisfies attach over ordinary terminals and SSH, is easy to test, and keeps the scope focused on human-readable event presentation.
  Date: 2026-04-28

- Decision: Keep `--raw` and `--json` output modes after adding pretty rendering.
  Rationale: Pretty output is for humans; raw and JSON modes are needed for debugging, automation, and future pi skill integration.
  Date: 2026-04-28

- Decision: Treat unknown pi events as printable fallback events.
  Rationale: Pi RPC may evolve. Operators should see that something happened rather than losing events or crashing attachment.
  Date: 2026-04-28

- Decision: Keep `events <session-id>` compact by default and add `events --pretty`.
  Rationale: `events` already has a useful compact replay behavior. Changing it by default would create unnecessary compatibility risk for a phase whose main user-visible goal is `attach`.
  Date: 2026-04-28

- Decision: Normalize tool fields before implementing the renderer.
  Rationale: Rendering directly from ad hoc raw JSON would make tool output brittle and would force renderer code to know too much about pi payload variations. Additive optional fields keep the protocol backward-compatible while giving the renderer stable inputs.
  Date: 2026-04-28

- Decision: Use renderer output chunks rather than only complete strings.
  Rationale: Pi assistant text arrives as deltas. Chunked output lets live attach print continuous assistant text while preserving deterministic tests through a pure transcript helper.
  Date: 2026-04-28

## Outcomes & Retrospective

(To be filled at completion. Include representative before/after output, the final test pass count, and any known renderer gaps.)

## Context and Orientation

Scherzo's EventHub stores `session.Event` records. The local control API serves those records. `scherzoctl` is the CLI client. The relevant files in the current tree are:

- `src/scherzo/session/event.gleam` defines `SessionSummary`, `SessionEvent`, `EventPayload`, `EventKind`, `EventPage`, and status/kind string helpers.
- `src/scherzo/session/json.gleam` encodes session summaries, events, payloads, pages, token totals, and redacted raw JSON.
- `src/scherzo/agent/pi_rpc.gleam` decodes line-delimited pi RPC JSON into `RpcRecord` values.
- `src/scherzo/agent/runner.gleam` converts `RpcRecord` values into `PiUpdate` values and redacts assistant message deltas.
- `src/scherzo/orchestrator/daemon.gleam` converts `PiUpdate` values into `session.EventPayload` values and publishes them to the EventHub.
- `src/scherzo/control/protocol.gleam` encodes and decodes control API requests and responses.
- `src/scherzo/control/client.gleam` provides `get_session`, `get_events`, `stream_events`, `compact_event_line`, and raw request helpers for `scherzoctl`.
- `src/scherzo/ctl.gleam` parses and runs `scherzoctl` commands. Before this phase, `attach` requires `--raw`.
- `test/fixtures/fake_pi_rpc.sh` is the fake pi process used by tests.
- `test/ctl_test.gleam`, `test/session_event_test.gleam`, `test/control_protocol_test.gleam`, `test/pi_rpc_test.gleam`, and daemon/control tests cover existing behavior.

A turn is one pi agent turn. Pi events such as `turn_start` and `turn_end` mark turn boundaries. Assistant text usually arrives through message events such as `message_start`, `message_update`, and `message_end`; the fake fixture currently emits `message_update`. Tool activity may arrive through events such as `tool_execution_start`, `tool_execution_update`, and `tool_execution_end`. UI events arrive as `extension_ui_request` and `extension_ui_response`. Token totals arrive from Scherzo after `get_session_stats` as a `turn_finished` / `TokenStats` session event.

The renderer introduced here is pure. It receives prior renderer state and one session event, and returns new renderer state plus zero or more output chunks. The CLI owns terminal I/O and streaming.

## Preconditions and Verified Facts

Before implementing this plan, verify these facts against the current repository and update this section if they have drifted:

- `src/scherzo/session/event.gleam` defines `EventPayload` with fields for kind, name, turn, pi type, message, request id, method, tool name, tokens, and redacted raw JSON.
- `src/scherzo/control/client.gleam` exposes `get_session`, `get_events`, and `stream_events`.
- `src/scherzo/ctl.gleam` currently parses `events <session-id>`, `events <session-id> --json`, and `attach --raw <session-id>`.
- `client.compact_event_line` is the existing compact raw line format and must remain the raw output source.
- `protocol.stream_event_to_string("1", stored_event)` is the existing attach JSON stream envelope and must remain the attach JSON output source.
- `test/fixtures/fake_pi_rpc.sh` currently emits `agent_start`, `turn_start`, `message_update`, optional `extension_ui_request`, `turn_end`, and `agent_end`, but not tool events.
- `direnv exec . gleam test` passes before implementation begins. Record the pass count in Progress.

If any verified fact is false, stop and update the plan before changing code.

## Scope Boundaries

In scope: additive event-field normalization for assistant/tool rendering; pure terminal renderer; ANSI color/style helpers; pretty/default `attach`; compact/default `events`; `events --pretty`; output modes `--raw`, `--json`, and `--pretty`; flags `--color=auto|always|never`, `--no-follow`, and `--since-cursor`; tests for rendering behavior, CLI mode selection, raw/JSON compatibility, replay/follow duplicate suppression, and truncated-history warnings; README examples.

Out of scope: mutating controls; input prompts inside attach; answering UI requests; abort/retry/pause/resume commands; full-screen TUI; terminal resizing; scrollback management; search; persistence of rendered transcripts; durable event storage; changing EventHub retention; reuse of pi's internal renderer unless it is already available as a stable import in the repository.

Auto-retry and compaction events are not invented by this phase. If pi emits concrete retry or compaction event names and those names are visible in the raw event stream, render them as status lines when practical and record the observed names in Surprises & Discoveries. If the current pi payload has no stable retry/compaction fields, the fallback `event <type>` rendering is acceptable for this phase.

## Milestones

Milestone 1 normalizes event fields and preserves protocol compatibility. At the end, assistant message events are classified as `AssistantMessage`, tool events are classified as `Tool`, optional tool input/output/status fields survive JSON encode/decode, fake pi can emit deterministic tool events, and all existing tests still pass.

Milestone 2 adds pure rendering primitives. At the end, tests can render a session header, truncation warning, turn start/end, continuous assistant message deltas, token summaries, and unknown events without starting a control server.

Milestone 3 adds tool and UI rendering. At the end, fake tool events render as labeled tool blocks, blocking UI requests render as operator-needed lines, UI responses render as completion lines when shown, and known status events render distinctly from assistant text.

Milestone 4 refactors `scherzoctl` option parsing and test seams. At the end, command parsing has explicit output modes, raw/JSON contracts are testable, and attach rendering can be exercised with fake client dependencies without opening a socket.

Milestone 5 wires renderer output into `scherzoctl` and documents usage. At the end, `attach` pretty output is the default, `attach --raw` preserves compact line mode, `attach --json` preserves stream-event JSON envelopes, `events` stays compact by default, `events --pretty` uses the renderer without following, and README shows representative output.

## Plan of Work

Extend `src/scherzo/session/event.gleam` additively. Keep existing fields and add optional tool fields to `EventPayload`: `tool_input: Option(String)`, `tool_output: Option(String)`, and `tool_status: Option(String)`. `tool_input` is the command, arguments, or input excerpt associated with a tool start event. `tool_output` is output or progress text associated with a tool update event. `tool_status` is a short status associated with a tool end event, such as `success`, `failed`, or `cancelled`. Update all constructors in tests and source code to populate these fields with `None` when not relevant.

Update `src/scherzo/session/json.gleam` and `src/scherzo/control/protocol.gleam` decoders so the new tool fields are optional on input and present as nullable fields on output. Existing JSON without those fields must still decode to `None`. Add tests for both old JSON and new JSON.

Extend `src/scherzo/agent/pi_rpc.gleam`. Add `tool_name`, `tool_input`, `tool_output`, and `tool_status` fields to `RpcRecord`. Decode `tool_name` from the first non-empty top-level field among `toolName`, `tool_name`, and `name` when the event type starts with `tool_execution_`. Decode `tool_input` from the first non-empty top-level string among `command`, `input`, and `args`. Decode `tool_output` from the first non-empty top-level string among `output`, `stdout`, `stderr`, and, for `tool_execution_update` only, `delta`. Decode `tool_status` from the first non-empty top-level string among `status` and `result`; if no string is present but a top-level boolean `success` is present, map `True` to `success` and `False` to `failed`.

Update `src/scherzo/agent/runner.gleam` so `PiUpdate` carries the new tool fields. Redact tool input and output with the same secret list used for assistant messages before storing them. Keep `message` as the assistant delta for `message_*` events; do not overload it with tool output.

Update `src/scherzo/orchestrator/daemon.gleam` so `kind_for_update` maps `message_start`, `message_update`, and `message_end` to `session_event.AssistantMessage`; maps `tool_execution_start`, `tool_execution_update`, and `tool_execution_end` to `session_event.Tool`; keeps `turn_finished` as `TokenStats`; keeps blocking `extension_ui_request` as `UiRequest`; and keeps unknown raw events as `PiRaw`. Update `update_payload` to copy the new tool fields.

Create `src/scherzo/terminal/style.gleam`. Define `ColorMode` as `ColorAuto | ColorAlways | ColorNever`. In this phase, `ColorAuto` behaves like `ColorNever` unless a terminal-detection helper is added and tested. Define pure helpers for headings, dim text, success, warning, error, assistant label, and tool label.

Create `src/scherzo/terminal/render.gleam`. Define `RenderChunk` as `Line(String)` or `Inline(String)`. `Line(text)` appends `text` and a newline to a transcript. `Inline(text)` appends text without a newline. Define a `chunks_to_string(chunks: List(RenderChunk)) -> String` helper for tests and command output. Define `RenderState(last_cursor, current_turn, assistant_open, active_tool_label)` and `RenderOptions(color_mode, show_lifecycle, show_raw_unknown)`.

Define renderer event semantics exactly. `render_event` returns no chunks and leaves state unchanged when `event.cursor <= state.last_cursor`. For `turn_start`, close any open assistant text, set `current_turn`, and render a turn heading. For the first assistant message delta in a turn, render an assistant label line, then an inline indented delta. For following assistant deltas, render only inline delta text. Before rendering any non-assistant event while `assistant_open` is `True`, emit an empty `Line("")` chunk to terminate the assistant text. For `turn_end`, close assistant text, render a turn-ended line, and keep token rendering separate unless the event itself has nonzero tokens. For `TokenStats`, render `tokens: input=<n> output=<n> cache_read=<n> cache_write=<n> total=<n>` when totals are nonzero. For unknown events, render `event <pi_type-or-name>` and optionally a compact redacted raw JSON excerpt.

Add a page-level helper such as `render_page(summary, page, options)` or keep the truncation warning in `ctl.gleam`, but specify that pretty replay must print a warning before events when `page.truncated` is `True`.

Modify `test/fixtures/fake_pi_rpc.sh` to support tool events with `FAKE_PI_TOOL=1`. When that variable is present, emit these events between the assistant `message_update` and `turn_end`:

    {"type":"tool_execution_start","toolName":"bash","command":"gleam test"}
    {"type":"tool_execution_update","toolName":"bash","output":"2 failures"}
    {"type":"tool_execution_end","toolName":"bash","status":"failed"}

Keep existing fake pi behavior unchanged when `FAKE_PI_TOOL` is absent.

Refactor `src/scherzo/ctl.gleam` to make output modes explicit. Add types equivalent to:

    pub type OutputMode {
      Pretty
      Raw
      Json
    }

    pub type FollowMode {
      Follow
      NoFollow
    }

    pub type Command {
      Help
      Ping(control_file: Option(String), json: Bool)
      Ps(control_file: Option(String), json: Bool)
      Session(control_file: Option(String), json: Bool, session_id: String)
      Events(control_file: Option(String), mode: OutputMode, color: style.ColorMode, since_cursor: Int, session_id: String)
      Attach(control_file: Option(String), mode: OutputMode, color: style.ColorMode, follow: FollowMode, since_cursor: Int, session_id: String)
    }

`attach <session-id>` defaults to `Pretty`, `Follow`, `since_cursor: 0`, and `ColorAuto`. `events <session-id>` defaults to `Raw`, `NoFollow`, `since_cursor: 0`, and `ColorNever`. `--raw`, `--json`, and `--pretty` are mutually exclusive. `--color=auto|always|never` only affects pretty mode; raw and JSON modes must never emit ANSI. `--no-follow` is valid for `attach` and has no effect on `events` because `events` never follows. `--since-cursor <n>` is valid for both `events` and `attach`; reject negative or non-integer cursors with a usage error.

Add a test seam in `src/scherzo/ctl.gleam` before writing attach integration tests. Introduce public or testable dependency records equivalent to a `ControlClient` with `get_session`, `get_events`, `stream_events`, and `raw_request` functions, and an `Output` record with `line` and `inline` functions. Keep `main` wired to the real `client` and `io` modules. Tests can provide fake dependencies and capture output without opening a socket.

Wire pretty `attach` as follows. Load the control file. Fetch `get_session(session_id)`; if missing, return `missing_session`. Fetch replayed events with `get_events(session_id, since_cursor, 200)`. Print the header, then a truncation warning if needed, then render replayed events through one `RenderState`. If following, call `stream_events(session_id, state.last_cursor, callback)` and pass every streamed event through the same state. The callback prints chunks as they arrive and continues until the server closes the stream or an error occurs.

Wire raw and JSON modes exactly. Raw mode prints `client.compact_event_line(stored_event)` for both replayed and streamed events. JSON attach mode prints `protocol.stream_event_to_string("1", stored_event)` for both replayed and streamed events, one JSON object per event, with no header and no ANSI. `events --json` keeps using `protocol.GetEvents` through `print_raw`, preserving the protocol response envelope. `events --pretty` fetches the session summary, fetches one event page, renders a header, prints a truncation warning if needed, and does not follow.

Update `README.md` with examples for `scripts/scherzoctl ps`, `scripts/scherzoctl attach`, `scripts/scherzoctl attach --raw`, `scripts/scherzoctl attach --json`, `scripts/scherzoctl events`, `scripts/scherzoctl events --pretty`, and `scripts/scherzoctl events --json`.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current pass count in Progress. Stop if existing tests fail.

2. Update `src/scherzo/session/event.gleam` to add `tool_input`, `tool_output`, and `tool_status` optional fields to `EventPayload`; update `empty_payload` to set all three to `None`.

3. Update constructors in source and tests that create `EventPayload` values so they pass the new fields as `None` unless the event is a tool event.

4. Add tests in `test/session_event_test.gleam` or `test/control_protocol_test.gleam` that encode and decode an event with `tool_name: Some("bash")`, `tool_input: Some("gleam test")`, `tool_output: Some("2 failures")`, and `tool_status: Some("failed")`.

5. Add a compatibility test that decodes an old event JSON payload with no `tool_input`, `tool_output`, or `tool_status` fields and asserts all three fields become `None`.

6. Update `src/scherzo/session/json.gleam` and `src/scherzo/control/protocol.gleam` until the new and old event JSON tests pass.

7. Add pi RPC decoder tests in `test/pi_rpc_test.gleam` for the three fake tool event shapes listed in Plan of Work. Assert `tool_name`, `tool_input`, `tool_output`, and `tool_status` decode correctly.

8. Update `src/scherzo/agent/pi_rpc.gleam` to add and decode the new `RpcRecord` fields until the pi RPC tests pass.

9. Update `src/scherzo/agent/runner.gleam` so `PiUpdate` carries the new tool fields and redacts tool input/output. Add or update a runner test that proves a secret in tool output is redacted before it reaches `PiUpdate`.

10. Update `src/scherzo/orchestrator/daemon.gleam` so assistant and tool events are classified as described in Plan of Work. Add or update a daemon/session event test proving `message_update` becomes `AssistantMessage` and `tool_execution_start` becomes `Tool`.

11. Update `test/fixtures/fake_pi_rpc.sh` so `FAKE_PI_TOOL=1` emits deterministic tool start/update/end events without affecting existing tests.

12. Run `direnv exec . gleam test`. Commit this normalization milestone only after tests pass. Suggested commit message: `Normalize session events for attach rendering`.

13. Create `test/terminal_style_test.gleam`. Add `color_never_outputs_plain_text_test`, asserting that styling helpers return no escape characters when color mode is `ColorNever`. Add `color_always_wraps_heading_test`, asserting that heading style contains an ANSI escape prefix when color mode is `ColorAlways`. Add `color_auto_is_plain_without_tty_helper_test` if `ColorAuto` is defined as plain in this phase.

14. Implement `src/scherzo/terminal/style.gleam` until the style tests pass.

15. Create `test/terminal_renderer_test.gleam`. Add `renders_session_header_test`, constructing a summary for `ABC-123` and asserting `chunks_to_string` output contains the issue identifier, title, workspace path, session id, and status.

16. In `test/terminal_renderer_test.gleam`, add `warns_when_replay_page_is_truncated_test`: pass an `EventPage` with `truncated: True` through the pretty page helper and assert output contains `older retained events were dropped`.

17. Add `groups_turn_and_assistant_delta_test`: feed `turn_start`, two `message_update` events with deltas `hello ` and `world`, and `turn_end`; assert transcript contains one turn heading, one assistant label, contiguous `hello world`, and a turn-ended line.

18. Add `suppresses_duplicate_cursor_test`: render an event with cursor `2`, then render another event with cursor `2` and different text; assert the second text is absent and `last_cursor` remains `2`.

19. Add `renders_tool_execution_block_test`: feed fake `tool_execution_start`, `tool_execution_update`, and `tool_execution_end` events; assert output contains `tool`, `bash`, `gleam test`, `2 failures`, and `failed`.

20. Add `renders_ui_request_and_unknown_event_test`: feed `extension_ui_request` with method `confirm` and an unknown pi type with redacted raw JSON; assert output shows `UI request`, `confirm`, and a fallback `event <type>` line without crashing.

21. Add `renders_token_summary_test`: feed a token stats event with input `1`, output `2`, cache values `0`, and total `3`; assert output contains `tokens` and `total=3`.

22. Implement `src/scherzo/terminal/render.gleam` until renderer tests pass with color disabled.

23. Refactor `src/scherzo/ctl.gleam` command types and parsing. Add parser tests in `test/ctl_test.gleam` for `attach ABC-123`, `attach --raw ABC-123`, `attach --json ABC-123`, `attach --no-follow ABC-123`, `attach --since-cursor 40 ABC-123`, `attach --color=never ABC-123`, `events ABC-123`, `events --pretty ABC-123`, `events --json ABC-123`, mutually exclusive output flags, negative cursor, non-integer cursor, and invalid color mode.

24. Add the `ControlClient` and `Output` test seam in `src/scherzo/ctl.gleam`. Keep `main` and normal `run` behavior wired to real control client and terminal I/O.

25. Add a CLI attach rendering test using fake dependencies. Fake `get_session` returns a summary, fake `get_events` returns replayed turn/tool events with `next_cursor: 2`, and fake `stream_events` invokes the callback with cursor `2` and cursor `3`. Assert default pretty output contains a header, assistant label, tool label, and the cursor `2` event only once.

26. Add raw/JSON compatibility tests. Assert `attach --raw --no-follow ABC-123` output equals `client.compact_event_line` for the fake event. Assert `attach --json --no-follow ABC-123` output parses as JSON with top-level `version`, `id`, `stream`, `session_id`, `cursor`, and `event`, and contains no ANSI escape. Assert `events --json ABC-123` still uses the protocol response envelope rather than pretty output.

27. Implement `src/scherzo/ctl.gleam` command execution until parser, fake dependency, raw compatibility, JSON compatibility, and existing tests pass.

28. Add or update a daemon/control integration test proving `FAKE_PI_TOOL=1` tool events survive from fake pi through EventHub and render as tool blocks through the control-client path.

29. Update `README.md` with a sample attach transcript. Keep it concise and use fake issue identifiers such as `ABC-123`.

30. Run `direnv exec . gleam format`, `direnv exec . gleam format --check src test`, and `direnv exec . gleam test`. Record the final pass count in Progress.

31. Commit the renderer and CLI phase after validation passes. Suggested commit message: `Render Scherzo session attach output`.

## Testing and Falsifiability

The renderer is falsified if assistant deltas are printed as disconnected JSON fragments, if two assistant deltas cannot render as contiguous text, if tool events cannot be distinguished from assistant text when normalized tool fields are present, if unknown events crash rendering, if truncated retained history is not visible in pretty replay, if replay/follow duplicates cursor output, if raw output differs from `client.compact_event_line`, if attach JSON output differs from `protocol.stream_event_to_string`, if `events --json` stops using the protocol response envelope, or if color cannot be disabled for tests and logs.

Add deterministic tests in `test/session_event_test.gleam`, `test/control_protocol_test.gleam`, `test/pi_rpc_test.gleam`, `test/terminal_style_test.gleam`, `test/terminal_renderer_test.gleam`, `test/ctl_test.gleam`, and one daemon/control integration test that exercises tool events through fake pi and the EventHub. No test may require a real terminal, real Linear, or real pi.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Before implementation, tests that import `scherzo/terminal/render` should fail to compile. After implementation, all tests should pass. Record both the initial and final pass counts in Progress.

## Validation and Acceptance

Automated acceptance is required: run the format check and full test suite from the repository root and accept only if both pass.

Manual acceptance is optional but recommended when a configured test Linear project and disposable test repository are available. From the repository root, create or edit a temporary workflow file under the repository, for example `.scherzo/tmp/attach-renderer-workflow.md`, using `examples/WORKFLOW.md` as the template. Set `pi.command` in that temporary workflow to `test/fixtures/fake_pi_rpc.sh`, keep `handoff.enabled: false`, set `workspace.root` to `.scherzo/workspaces`, and use a disposable `REPO_URL`. Then start daemon mode with tool events enabled:

    FAKE_PI_TOOL=1 LINEAR_API_KEY=lin_api_for_test_project REPO_URL=git@example.com:org/disposable-test-repo.git direnv exec . gleam run -- .scherzo/tmp/attach-renderer-workflow.md

In another terminal, use the logged control file or export it:

    export SCHERZO_CONTROL_FILE=.scherzo/workspaces/.scherzo-state/control.json
    scripts/scherzoctl ping
    scripts/scherzoctl ps
    scripts/scherzoctl attach --no-follow <session-id>

Accept pretty attach only if output shows a header with issue/workspace/session, turn boundaries, assistant text, a `tool bash` block with `gleam test` and `2 failures`, and final token/status information. Then run:

    scripts/scherzoctl attach --json --no-follow <session-id>
    scripts/scherzoctl attach --raw --no-follow <session-id>
    scripts/scherzoctl events <session-id>
    scripts/scherzoctl events --pretty <session-id>
    scripts/scherzoctl events --json <session-id>

Accept only if JSON and raw modes still work, contain no ANSI escapes, and remain suitable for automation/debugging.

## Rollout, Recovery, and Idempotence

This phase changes presentation and additive event fields only. It does not change scheduler state, worker behavior, control authentication, Linear interactions, or EventHub retention. If the pretty renderer is wrong, operators can use `attach --raw`, `attach --json`, default `events`, or `events --json` while the renderer is fixed.

The change is reversible at the CLI level because raw and JSON modes remain available. Additive event fields are backward-compatible: older JSON without the fields decodes with `None`, and newer JSON includes nullable fields that older consumers should ignore if they parse only known keys.

Renderer functions are pure and idempotent for the same input events and starting state. Attach clients track last cursor locally; reconnecting and replaying from an older cursor may print older retained events again, which is expected. During one attach invocation, duplicate cursor suppression prevents replay/live handoff duplicates.

## Artifacts and Notes

Representative target output:

    ABC-123 Fix parser tests
    workspace: .scherzo/workspaces/ABC-123
    session: ABC-123-1714320000000
    status: running
    ------------------------------------------------------------

    ▶ turn 1 started

    assistant
      I'll inspect the failing parser tests.

    tool bash
      $ gleam test
      2 failures
      ✗ failed

    ✓ turn 1 ended
    tokens: input=1200 output=300 cache_read=0 cache_write=0 total=1500

If retained history is truncated, pretty output includes a warning before event rendering:

    warning: older retained events were dropped before this replay window

Exact wording may differ, but the output must make turn, assistant, tool, UI, fallback events, truncation, and token events visually distinct.

## Interfaces and Dependencies

In `src/scherzo/session/event.gleam`, `EventPayload` must end with fields equivalent to:

    message: Option(String),
    request_id: Option(String),
    method: Option(String),
    tool_name: Option(String),
    tool_input: Option(String),
    tool_output: Option(String),
    tool_status: Option(String),
    tokens: domain.TokenTotals,
    raw_json: Option(RedactedRawJson),

In `src/scherzo/terminal/style.gleam`, expose functions equivalent to:

    pub type ColorMode {
      ColorAuto
      ColorAlways
      ColorNever
    }

    pub fn heading(mode: ColorMode, text: String) -> String
    pub fn dim(mode: ColorMode, text: String) -> String
    pub fn success(mode: ColorMode, text: String) -> String
    pub fn warning(mode: ColorMode, text: String) -> String
    pub fn error(mode: ColorMode, text: String) -> String
    pub fn assistant_label(mode: ColorMode, text: String) -> String
    pub fn tool_label(mode: ColorMode, text: String) -> String

In `src/scherzo/terminal/render.gleam`, expose functions equivalent to:

    pub type RenderChunk {
      Line(String)
      Inline(String)
    }

    pub type RenderState {
      RenderState(
        last_cursor: Int,
        current_turn: Option(Int),
        assistant_open: Bool,
        active_tool_label: Option(String),
      )
    }

    pub type RenderOptions {
      RenderOptions(
        color_mode: style.ColorMode,
        show_lifecycle: Bool,
        show_raw_unknown: Bool,
      )
    }

    pub fn initial_state(since_cursor: Int) -> RenderState
    pub fn default_options(color_mode: style.ColorMode) -> RenderOptions
    pub fn chunks_to_string(chunks: List(RenderChunk)) -> String
    pub fn render_header(summary: event.SessionSummary, options: RenderOptions) -> List(RenderChunk)
    pub fn render_truncation_warning(options: RenderOptions) -> List(RenderChunk)
    pub fn render_event(state: RenderState, event: event.SessionEvent, options: RenderOptions) -> #(RenderState, List(RenderChunk))
    pub fn render_events(state: RenderState, events: List(event.SessionEvent), options: RenderOptions) -> #(RenderState, List(RenderChunk))

No new runtime dependency should be required. Use only existing JSON/session/control data and small ANSI helpers. If terminal detection for `ColorAuto` is added, implement it as a small isolated helper with tests; otherwise document and test that `ColorAuto` behaves as no color in this phase.
