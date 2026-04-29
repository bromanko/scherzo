# Add a human-readable terminal attach renderer

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, an operator can attach to a Scherzo worker from a terminal and understand what the agent is doing without reading raw JSON or compact event lines. The visible proof is that `scherzoctl attach <session-id>` replays recent retained events and follows new ones, grouping output by turn, rendering assistant text as assistant text, rendering tool executions as tool blocks when tool fields are present, showing blocking UI requests distinctly, warning when retained history is truncated, and printing a concise token summary at turn end. `scherzoctl attach --raw <session-id>` continues to expose compact event lines for debugging, `scherzoctl attach --json <session-id>` continues to expose one structured JSON object per event for automation, and the existing `scherzoctl attach --raw --json <session-id>` invocation remains accepted as a legacy alias for JSON attach so existing scripts do not break.

This phase changes presentation only. It does not add mutating controls, does not answer UI requests, does not abort or retry workers, and does not change worker behavior.

## Problem Framing and Constraints

The local read-only control API makes session data available, but raw event streams are too low-level for everyday operation. Operators need to answer questions like: Is the worker thinking, editing, running a shell command, waiting on a UI request, compacting context, retrying inside pi, or done? Pi's default interactive rendering is good at this, but Scherzo currently stores and serves pi RPC events through its own `session.Event` model. Unless pi exposes a reusable renderer library in this repository, Scherzo needs its own terminal renderer that preserves the same spirit: grouped turns, clear role labels, tool boundaries, and useful progress without overwhelming noise.

The renderer must be robust to unknown pi event types, partial retained history, replay followed by live streaming, colorless terminals, narrow terminals, and automation users that depend on raw or JSON output. It must not change session state, worker behavior, control authentication, Linear interactions, or the EventHub retention policy.

`events <session-id>` is intentionally not changed to pretty output by default in this phase. It keeps the existing compact line output for compatibility with operators and lightweight scripts. Pretty replay is available as `events --pretty <session-id>`.

## Strategy Overview

First verify the real event data that the renderer will depend on. Before changing normalization, capture at least one real pi RPC transcript that includes a tool execution, preferably by inspecting existing pi agent logs under `$HOME/.pi/agent`, or use a checked-in captured JSONL fixture made from such a transcript. If existing logs do not contain suitable tool events, run the configured pi command in a disposable workspace to generate one. The fake fixture must mirror the captured real field shapes rather than inventing convenient tool events. If neither existing logs, a runnable real pi command, nor a captured real transcript is available, stop before normalizing tool fields and ask for a fixture; fake-only tool shapes are not enough to prove the renderer's main promise.

Then normalize the event data that the renderer needs. The current tree already has `src/scherzo/session/event.gleam`, `src/scherzo/session/json.gleam`, `src/scherzo/agent/pi_rpc.gleam`, `src/scherzo/agent/runner.gleam`, `src/scherzo/orchestrator/daemon.gleam`, `src/scherzo/control/client.gleam`, and `src/scherzo/ctl.gleam`. The renderer depends on those modules preserving enough structured fields to identify assistant messages, tool execution start/update/end events, UI requests, token totals, and redacted raw fallbacks. Tool and assistant classification must be fixed before the terminal renderer is wired into the CLI.

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

The main data-shape risk is that current Scherzo events do not normalize tool fields and the exact real pi payload shape may differ from early guesses. Countermeasure: start with a payload-discovery spike using real pi output or a captured real JSONL fixture, then update fake pi fixtures and decoder tests from that evidence before implementing the renderer. Unknown events still render through the fallback path, but acceptance for tool blocks requires tests based on observed real tool payloads.

The main compatibility risk is breaking existing automation while making pretty attach the default. Countermeasure: keep raw and JSON output stable, keep default `events` compact, add `events --pretty` instead of changing `events`, and preserve the existing `attach --raw --json <session-id>` invocation as a legacy alias for JSON attach. The parser rejects other conflicting pretty/raw/json flag combinations.

The main compatibility risk for pi itself is unknown or changing event types. Countermeasure: every event has a fallback renderer that prints `event <pi_type-or-name>` and, when `show_raw_unknown` is enabled, a compact redacted raw payload excerpt. Unknown events must never crash attach.

The main terminal risk is ANSI color causing unreadable logs or test instability. Countermeasure: support `--color=auto|always|never`; renderer tests use `ColorNever`; `attach --json`, `attach --raw`, `events --json`, and default `events` never emit ANSI. In this phase, `ColorAuto` is defined as no color unless a small terminal-detection helper is implemented and tested.

The main stream risk is duplicate output when replay transitions to live follow. Countermeasure: all attach modes first fetch retained replay pages while tracking the last printed cursor, then follow with `stream_events(session_id, last_cursor)`. Pretty mode also has renderer-level duplicate suppression. Raw and JSON modes use the same cursor handoff and skip any streamed event with `cursor <= last_printed_cursor`.

The main replay risk is silently omitting useful events when one retained page is not enough. Countermeasure: pretty attach, raw/json attach, and `events --pretty` fetch replay pages until a page returns fewer than the page size, returns no events, or fails to advance `next_cursor`. Default compact `events` and `events --json` keep their existing one-page response shape for compatibility.

The main retained-history risk is misleading output when old events have been dropped by EventHub retention or the user starts from a cursor in the middle of a turn. Countermeasure: if any replay page reports `truncated: True`, pretty replay prints a visible warning before rendered events. If rendering begins with an event that has a turn number but no preceding `turn_start`, the renderer prints a `turn <n> continued` heading before assistant/tool/UI output.

The main output-size risk is flooding terminals or retained session JSON with huge tool input/output. Countermeasure: normalized `tool_input`, `tool_output`, and `tool_status` values are redacted and capped before storage. Long values use a visible suffix such as `… [truncated]`; non-string structured input is summarized as structured input rather than dumped wholesale unless a captured real pi payload proves a safer compact representation.

The main redaction risk is leaking secrets through newly normalized tool fields. Countermeasure: every decoded tool input/output/status string is passed through the same secret-redaction path used for assistant deltas before it becomes a `session.EventPayload`; raw JSON remains recursively redacted and capped as it is today.

## Progress

- [x] (2026-04-28 18:40Z) Confirmed this phase depends on the read-only control API and does not add mutating controls.
- [x] (2026-04-28 19:20Z) Revised the plan after adversarial review to keep `events` compact by default, normalize tool fields before rendering, define output chunk semantics, preserve exact raw/JSON contracts, and add replay/follow duplicate tests.
- [x] (2026-04-29 02:45Z) Revised the plan after follow-up review to preserve the legacy `attach --raw --json` alias, add a real pi payload-discovery milestone, define paginated replay and mid-turn retained-history behavior, cap normalized tool text, and make the `ctl.gleam` test seam explicit.
- [x] (2026-04-29 02:53Z) Updated the payload-discovery path to prefer existing pi agent logs under `$HOME/.pi/agent` before requiring a new real-pi run or user-supplied transcript.
- [x] (2026-04-29 03:05Z) Ran `direnv exec . gleam test` before implementation work; the existing suite was green, and the compatibility baseline observed during the first successful post-normalization run was 200 passed tests.
- [x] (2026-04-29 03:15Z) Added `test/fixtures/pi_tool_events_captured.jsonl` from redacted existing pi agent logs containing assistant `toolCall` and `toolResult` message shapes.
- [x] (2026-04-29 03:45Z) Normalized assistant and tool fields through pi RPC, runner updates, daemon EventHub payloads, session JSON, and control protocol decoders.
- [x] (2026-04-29 04:05Z) Added pure ANSI/style helpers and renderer output chunk/state types under `src/scherzo/terminal/`.
- [x] (2026-04-29 04:35Z) Added renderer and CLI tests covering turn grouping, continued mid-turn replay, assistant deltas, tool events, UI requests, unknown events, truncation warnings, duplicate cursors, paginated replay, and token summaries.
- [x] (2026-04-29 05:00Z) Refactored `scherzoctl` command options and test seams while preserving the legacy `attach --raw --json` alias.
- [x] (2026-04-29 05:20Z) Updated `scherzoctl attach` to use pretty rendering by default and added paginated `events --pretty`.
- [x] (2026-04-29 05:30Z) Updated README documentation with terminal attach examples.

## Surprises & Discoveries

- Existing pi agent logs contained tool activity as `type: "message"` records rather than only `tool_execution_*` records. Assistant tool calls appeared as `message.role: "assistant"` with `content[].type: "toolCall"`, `content[].name`, and nested `content[].arguments.command`. Tool results appeared as `message.role: "toolResult"` with `toolName`, `content[].text`, and `isError`.
- The fake pi fixture now emits deterministic captured-style `type: "message"` tool-call and tool-result records when `FAKE_PI_TOOL=1`, while decoder tests also cover explicit `tool_execution_start`, `tool_execution_update`, and `tool_execution_end` top-level/data alias shapes for compatibility with the original expected taxonomy.
- Structured top-level tool inputs can appear in principle; they are normalized to `[structured tool input; use --json for raw details]` instead of being stringified into terminal output.
- `ColorAuto` intentionally behaves like `ColorNever` in this phase. That keeps tests and log captures stable until terminal detection is implemented deliberately.
- The `ctl.gleam` test seam now injects control-client and output functions. Live follow rendering keeps cursor/render state across synchronous stream callbacks so replay-to-follow duplicate suppression works for pretty, raw, and JSON modes.

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

- Decision: Preserve `attach --raw --json <session-id>` as a legacy alias for JSON attach.
  Rationale: The current CLI allows JSON stream output only through the raw attach command shape. Keeping the alias avoids breaking scripts while allowing the clearer new `attach --json <session-id>` form.
  Date: 2026-04-29

- Decision: Validate real pi tool payload shapes before normalizing tool fields.
  Rationale: Passing tests against invented fake tool events would not prove the renderer works for actual operators. A captured real transcript makes the fake fixture and decoder tests meaningful.
  Date: 2026-04-29

- Decision: Pretty replay and attach replay must paginate, while default compact `events` and `events --json` remain one-page compatibility surfaces.
  Rationale: Human pretty output should not silently omit the end of a retained history, but changing existing compact/JSON event replay shape risks breaking lightweight consumers.
  Date: 2026-04-29

- Decision: Render retained-history mid-turn starts as continued turns.
  Rationale: EventHub retention or `--since-cursor` can start replay inside a turn. A visible continued-turn heading preserves the grouping model even without the original `turn_start` event.
  Date: 2026-04-29

- Decision: Cap normalized tool text before storage.
  Rationale: Tool output can be much larger than assistant deltas. Redacted, visibly truncated excerpts are useful to humans without flooding terminal output, retained session JSON, or tests.
  Date: 2026-04-29

- Decision: Prefer existing pi agent logs under `$HOME/.pi/agent` for tool payload discovery.
  Rationale: The repository may not need a new real-pi run if existing local pi logs already contain representative tool execution events. The implementation should copy only redacted, minimal snippets into repository fixtures.
  Date: 2026-04-29

## Outcomes & Retrospective

Implemented the line-oriented attach renderer and wired it into `scherzoctl attach` as the default. `attach --raw`, `attach --json`, and the legacy `attach --raw --json` alias remain available; `events` remains compact by default, and `events --pretty` provides paginated human-readable replay.

Representative pretty output now looks like:

    ABC-123 Fix flaky tests
    workspace: /workspaces/ABC-123
    session: ABC-123-42-1
    status: running

    ▶ turn 1 started
    assistant:
      I will run the tests and inspect the failure.
    tool bash
      input: gleam test
      output: 2 failures
    tokens: input=1200 output=340 cache_read=0 cache_write=0 total=1540

The final verification command was `direnv exec . gleam test`, with 213 passed tests and no failures. Known gaps for future phases: retry/compaction pi events are still rendered through generic fallback unless pi exposes stable event names, `ColorAuto` does not yet inspect the terminal, and this remains a streaming line renderer rather than an interactive TUI.

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

- `src/scherzo/session/event.gleam` defines `EventPayload` with fields for kind, name, turn, pi type, message, request id, method, tool name, tokens, and redacted raw JSON. It does not yet define `tool_input`, `tool_output`, or `tool_status`.
- `src/scherzo/session/json.gleam` currently encodes session summaries, events, pages, payloads, tokens, and raw JSON. It does not contain event decoders; event/page decoding currently lives in `src/scherzo/control/protocol.gleam`.
- `src/scherzo/control/client.gleam` exposes `get_session`, `get_events`, and `stream_events`.
- `src/scherzo/ctl.gleam` currently parses `events <session-id>`, `events <session-id> --json`, `attach --raw <session-id>`, and `attach --raw --json <session-id>` for JSON stream output.
- `client.compact_event_line` is the existing compact raw line format and must remain the raw output source.
- `protocol.stream_event_to_string("1", stored_event)` is the existing attach JSON stream envelope and must remain the attach JSON output source.
- `test/fixtures/fake_pi_rpc.sh` currently emits `agent_start`, `turn_start`, `message_update`, optional `extension_ui_request`, `turn_end`, and `agent_end`, but not tool events.
- The implementer can inspect existing pi agent logs under `$HOME/.pi/agent`, run the configured real pi command in a disposable workspace, or use a user-provided captured JSONL transcript to create `test/fixtures/pi_tool_events_captured.jsonl`. If none of those sources is available, stop and ask for a captured real transcript before normalizing tool fields.
- `direnv exec . gleam test` passes before implementation begins. Record the pass count in Progress.

If any verified fact is false, stop and update the plan before changing code.

## Scope Boundaries

In scope: real-payload discovery for pi tool events; additive event-field normalization for assistant/tool rendering; redaction and length-capping of normalized tool text; pure terminal renderer; ANSI color/style helpers; pretty/default `attach`; compact/default `events`; `events --pretty`; output modes `--raw`, `--json`, and `--pretty`; legacy `attach --raw --json` as a JSON attach alias; flags `--color=auto|always|never`, `--no-follow`, and `--since-cursor`; paginated pretty replay; tests for rendering behavior, mid-turn retained-history replay, CLI mode selection, raw/JSON compatibility, replay/follow duplicate suppression in all attach modes, truncated-history warnings, and README examples.

Out of scope: mutating controls; input prompts inside attach; answering UI requests; abort/retry/pause/resume commands; full-screen TUI; terminal resizing; scrollback management; search; persistence of rendered transcripts; durable event storage; changing EventHub retention; changing the default one-page compact `events` or `events --json` response shape; reuse of pi's internal renderer unless it is already available as a stable import in the repository.

Auto-retry and compaction events are not invented by this phase. If pi emits concrete retry or compaction event names and those names are visible in the raw event stream, render them as status lines when practical and record the observed names in Surprises & Discoveries. If the current pi payload has no stable retry/compaction fields, the fallback `event <type>` rendering is acceptable for this phase.

## Milestones

Milestone 1 captures real pi payload evidence and establishes the compatibility baseline. At the end, the Progress section records the initial `direnv exec . gleam test` pass count, `test/fixtures/pi_tool_events_captured.jsonl` contains representative real tool execution events copied and redacted from `$HOME/.pi/agent`, a disposable real-pi run, or user-supplied captured lines, and parser tests document the existing `attach --raw --json` JSON-stream behavior that must remain valid.

Milestone 2 normalizes event fields and preserves protocol compatibility. At the end, assistant message events are classified as `AssistantMessage`, tool events are classified as `Tool`, optional tool input/output/status fields survive JSON encode/decode, normalized tool text is redacted and capped, fake pi emits deterministic tool events based on the captured real payload shape, and all existing tests still pass.

Milestone 3 adds pure rendering primitives. At the end, tests can render a session header, truncation warning, turn start/end, mid-turn continued headings, continuous assistant message deltas, token summaries, duplicate cursor suppression, paginated replay helpers, and unknown events without starting a control server.

Milestone 4 adds tool and UI rendering. At the end, fake tool events render as labeled tool blocks, blocking UI requests render as operator-needed lines, UI responses render as completion lines when shown, known status events render distinctly from assistant text, and large tool output renders as a redacted truncated excerpt.

Milestone 5 refactors `scherzoctl` option parsing and test seams. At the end, command parsing has explicit output modes, the legacy `attach --raw --json` alias maps to JSON mode, raw/JSON contracts are testable, replay pagination is reusable across modes, and attach rendering can be exercised with fake client dependencies without opening a socket.

Milestone 6 wires renderer output into `scherzoctl` and documents usage. At the end, `attach` pretty output is the default, `attach --raw` preserves compact line mode, `attach --json` and `attach --raw --json` preserve stream-event JSON envelopes, `events` stays compact by default, `events --pretty` uses paginated pretty replay without following, and README shows representative output.

## Plan of Work

Capture real pi tool payloads before normalizing. First inspect existing pi agent logs under `$HOME/.pi/agent` for recent JSONL or transcript files containing tool execution events. If those logs do not contain suitable events, use the configured pi command in a disposable workspace, or a user-provided transcript, to create `test/fixtures/pi_tool_events_captured.jsonl` containing at least one tool start-like event, one tool update/output-like event, and one tool end/result-like event. Keep only redacted, non-secret fixture content. Record the observed field names in Surprises & Discoveries. If the observed event names differ from `tool_execution_start`, `tool_execution_update`, and `tool_execution_end`, use the observed names in decoder tests and extend the renderer taxonomy accordingly rather than forcing the fake fixture to the guessed names.

Extend `src/scherzo/session/event.gleam` additively. Keep existing fields and add optional tool fields to `EventPayload`: `tool_input: Option(String)`, `tool_output: Option(String)`, and `tool_status: Option(String)`. `tool_input` is the command, arguments, or input excerpt associated with a tool start event. `tool_output` is output or progress text associated with a tool update event. `tool_status` is a short status associated with a tool end event, such as `success`, `failed`, or `cancelled`. Update all constructors in tests and source code to populate these fields with `None` when not relevant.

Update `src/scherzo/session/json.gleam` encoders so the new tool fields are present as nullable fields on output. Update `src/scherzo/control/protocol.gleam` event/page decoders so the new tool fields are optional on input. Existing JSON without those fields must still decode to `None`. Add tests for both old JSON and new JSON through the protocol decoders.

Extend `src/scherzo/agent/pi_rpc.gleam`. Add `tool_name`, `tool_input`, `tool_output`, and `tool_status` fields to `RpcRecord`. Base the exact aliases on `test/fixtures/pi_tool_events_captured.jsonl`. The initial alias set is: decode `tool_name` from the first non-empty top-level field among `toolName`, `tool_name`, and `name` when the event type starts with `tool_execution_`; decode `tool_input` from the first non-empty top-level string among `command`, `input`, and `args`; decode `tool_output` from the first non-empty top-level string among `output`, `stdout`, `stderr`, and, for `tool_execution_update` only, `delta`; decode `tool_status` from the first non-empty top-level string among `status` and `result`; if no string is present but a top-level boolean `success` is present, map `True` to `success` and `False` to `failed`. If the captured real payload stores equivalent fields under a top-level `data` object, add the same aliases under `data` as well. If an input-like field is present but structured rather than a string, normalize it to a short placeholder such as `[structured tool input; use --json for raw details]` unless a safe compact string representation is obvious from the captured payload and covered by tests.

Update `src/scherzo/agent/runner.gleam` so `PiUpdate` carries the new tool fields. Redact tool input, output, and status with the same secret list used for assistant messages before storing them. Cap each normalized tool string after redaction to a small deterministic limit, initially 4096 characters, and append `… [truncated]` when a value is shortened. Keep `message` as the assistant delta for `message_*` events; do not overload it with tool output.

Update `src/scherzo/orchestrator/daemon.gleam` so `kind_for_update` maps `message_start`, `message_update`, and `message_end` to `session_event.AssistantMessage`; maps `tool_execution_start`, `tool_execution_update`, and `tool_execution_end` to `session_event.Tool`; keeps `turn_finished` as `TokenStats`; keeps blocking `extension_ui_request` as `UiRequest`; and keeps unknown raw events as `PiRaw`. Update `update_payload` to copy the new tool fields.

Create `src/scherzo/terminal/style.gleam`. Define `ColorMode` as `ColorAuto | ColorAlways | ColorNever`. In this phase, `ColorAuto` behaves like `ColorNever` unless a terminal-detection helper is added and tested. Define pure helpers for headings, dim text, success, warning, error, assistant label, and tool label.

Create `src/scherzo/terminal/render.gleam`. Define `RenderChunk` as `Line(String)` or `Inline(String)`. `Line(text)` appends `text` and a newline to a transcript. `Inline(text)` appends text without a newline. Define a `chunks_to_string(chunks: List(RenderChunk)) -> String` helper for tests and command output. Define `RenderState(last_cursor, current_turn, assistant_open, active_tool_label)` and `RenderOptions(color_mode, show_lifecycle, show_raw_unknown)`.

Define renderer event semantics exactly. `render_event` returns no chunks and leaves state unchanged when `event.cursor <= state.last_cursor`. For `turn_start`, close any open assistant text, set `current_turn`, and render a turn heading. For any assistant/tool/UI/token event with `payload.turn: Some(n)` when `current_turn` is `None`, render a `turn n continued` heading before the event so truncated retained history still has a visible grouping boundary. For the first assistant message delta in a turn, render an assistant label line, then an inline indented delta. For following assistant deltas, render only inline delta text. Before rendering any non-assistant event while `assistant_open` is `True`, emit an empty `Line("")` chunk to terminate the assistant text. For `turn_end`, close assistant text, render a turn-ended line, and keep token rendering separate unless the event itself has nonzero tokens. For `TokenStats`, render `tokens: input=<n> output=<n> cache_read=<n> cache_write=<n> total=<n>` when totals are nonzero. For unknown events, render `event <pi_type-or-name>` and optionally a compact redacted raw JSON excerpt.

Add page-level helpers that make replay behavior explicit. `render_page(summary, page, options)` may render one page, but CLI pretty replay uses a pagination helper that repeatedly calls `get_events(session_id, cursor, 200)` until a page returns fewer than 200 events, returns no events, or returns a `next_cursor` that does not advance. Pretty replay prints one truncation warning before any event output if any fetched page has `truncated: True`.

Modify `test/fixtures/fake_pi_rpc.sh` to support tool events with `FAKE_PI_TOOL=1`. When that variable is present, emit deterministic tool events between the assistant `message_update` and `turn_end` using the same event names and field shapes recorded in `test/fixtures/pi_tool_events_captured.jsonl`. If the captured real shape matches the initial alias set, the fake events are:

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

`attach <session-id>` defaults to `Pretty`, `Follow`, `since_cursor: 0`, and `ColorAuto`. `events <session-id>` defaults to `Raw`, `NoFollow`, `since_cursor: 0`, and `ColorNever`. `--raw`, `--json`, and `--pretty` are mutually exclusive except for the attach-only legacy alias `attach --raw --json <session-id>`, which parses as `mode: Json`. `--color=auto|always|never` only affects pretty mode; raw and JSON modes must never emit ANSI. `--no-follow` is valid for `attach` and has no effect on `events` because `events` never follows. `--since-cursor <n>` is valid for both `events` and `attach`; reject negative or non-integer cursors with a usage error.

Add a test seam in `src/scherzo/ctl.gleam` before writing attach integration tests. Introduce public or testable dependency records with signatures equivalent to:

    pub type ControlClient {
      ControlClient(
        get_session: fn(file.ControlFile, String) -> Result(Option(event.SessionSummary), client.ControlError),
        get_events: fn(file.ControlFile, String, Int, Int) -> Result(event.EventPage, client.ControlError),
        stream_events: fn(file.ControlFile, String, Int, fn(event.SessionEvent) -> client.StreamAction) -> Result(Nil, client.ControlError),
        raw_request: fn(file.ControlFile, protocol.Request) -> Result(String, client.ControlError),
      )
    }

    pub type Output {
      Output(
        line: fn(String) -> Nil,
        inline: fn(String) -> Nil,
      )
    }

    pub fn run_with_deps(command: Command, deps: ControlClient, output: Output) -> Result(Nil, Error)

Keep `main` and the normal `run` wrapper wired to the real `client` and `io` modules. Tests provide fake dependencies and capture output without opening a socket. The fake `stream_events` dependency must be able to invoke the callback with duplicate and advancing cursors so replay/follow handoff behavior is testable.

Wire pretty `attach` as follows. Load the control file. Fetch `get_session(session_id)`; if missing, return `missing_session`. Fetch retained replay with a helper such as `fetch_replay_pages(deps, control_file, session_id, since_cursor, 200)`, which returns the concatenated events, the last cursor, and whether any page was truncated. Print the header, then one truncation warning if needed, then render replayed events through one `RenderState`. If following, call `stream_events(session_id, state.last_cursor, callback)` and pass every streamed event through the same state. The callback prints chunks as they arrive and continues until the server closes the stream or an error occurs.

Wire raw and JSON modes exactly. Raw mode prints `client.compact_event_line(stored_event)` for replayed and streamed events. JSON attach mode, including the legacy `attach --raw --json` alias, prints `protocol.stream_event_to_string("1", stored_event)` for replayed and streamed events, one JSON object per event, with no header and no ANSI. Raw and JSON attach use the same paginated replay helper as pretty attach and track `last_printed_cursor`; streamed events with `cursor <= last_printed_cursor` are skipped. `events --json` keeps using `protocol.GetEvents` through `print_raw`, preserving the protocol response envelope and one-page response shape. `events --pretty` fetches the session summary, fetches paginated retained replay, renders a header, prints one truncation warning if needed, and does not follow.

Update `README.md` with examples for `scripts/scherzoctl ps`, `scripts/scherzoctl attach`, `scripts/scherzoctl attach --raw`, `scripts/scherzoctl attach --json`, legacy `scripts/scherzoctl attach --raw --json`, `scripts/scherzoctl events`, `scripts/scherzoctl events --pretty`, and `scripts/scherzoctl events --json`.

## Concrete Steps

1. From the repository root, run `direnv exec . gleam test` and record the current pass count in Progress. Stop if existing tests fail.

2. Capture real pi tool payload evidence before code changes. First inspect existing pi agent logs under `$HOME/.pi/agent` for recent JSONL or transcript files containing tool execution events. If those logs are unsuitable, run the configured pi command in a disposable workspace with a prompt that causes a small shell/tool execution. If real pi is not available, create `test/fixtures/pi_tool_events_captured.jsonl` from user-provided captured lines. Redact secrets before writing the fixture. Stop and ask for captured lines only if none of these sources is available.

3. Read `test/fixtures/pi_tool_events_captured.jsonl` and record in Surprises & Discoveries the observed event names and field paths for tool start, update/output, and end/result. If no separate tool update or end exists in the capture, record that fact and adjust the normalization tests to the observed real lifecycle.

4. Add parser tests in `test/ctl_test.gleam` that document the current compatibility surface before the refactor: `attach --raw ABC-123` parses as raw attach, and `attach --raw --json ABC-123` parses to the existing raw-attach command with JSON enabled. These tests should pass before the parser refactor and remain passing after the alias is preserved in the new command model.

5. Update `src/scherzo/session/event.gleam` to add `tool_input`, `tool_output`, and `tool_status` optional fields to `EventPayload`; update `empty_payload` to set all three to `None`.

6. Update constructors in source and tests that create `EventPayload` values so they pass the new fields as `None` unless the event is a tool event.

7. Add tests in `test/session_event_test.gleam` or `test/control_protocol_test.gleam` that encode and decode an event with `tool_name: Some("bash")`, `tool_input: Some("gleam test")`, `tool_output: Some("2 failures")`, and `tool_status: Some("failed")`.

8. Add a compatibility test that decodes an old event JSON payload with no `tool_input`, `tool_output`, or `tool_status` fields through `protocol.decode_get_events_response` or `protocol.decode_stream_event` and asserts all three fields become `None`.

9. Update encoders in `src/scherzo/session/json.gleam` and event/page decoders in `src/scherzo/control/protocol.gleam` until the new and old event JSON tests pass.

10. Add pi RPC decoder tests in `test/pi_rpc_test.gleam` using records copied from `test/fixtures/pi_tool_events_captured.jsonl`. Assert `tool_name`, `tool_input`, `tool_output`, and `tool_status` decode according to the observed real field names. Include the initial top-level `toolName`/`command`/`output`/`status` fake shape only if it matches the capture or is kept as an additional compatibility alias.

11. Add a pi RPC decoder test for a structured input-like field if the capture includes one. Assert the normalized `tool_input` is a short placeholder such as `[structured tool input; use --json for raw details]`, not a raw object dump.

12. Update `src/scherzo/agent/pi_rpc.gleam` to add and decode the new `RpcRecord` fields until the pi RPC tests pass.

13. Update `src/scherzo/agent/runner.gleam` so `PiUpdate` carries the new tool fields, redacts tool input/output/status, and caps each normalized tool field to 4096 characters with a visible `… [truncated]` suffix when shortened. Add or update runner tests proving a secret in tool output is redacted and a long tool output is truncated before it reaches `PiUpdate`.

14. Update `src/scherzo/orchestrator/daemon.gleam` so assistant and tool events are classified as described in Plan of Work. Add or update a daemon/session event test proving `message_update` becomes `AssistantMessage` and the captured real tool-start event name becomes `Tool`.

15. Update `test/fixtures/fake_pi_rpc.sh` so `FAKE_PI_TOOL=1` emits deterministic tool start/update/end events matching the captured real field shape without affecting existing tests.

16. Run `direnv exec . gleam test`. Commit this discovery and normalization milestone only after tests pass. Suggested commit message: `Normalize session events for attach rendering`.

17. Create `test/terminal_style_test.gleam`. Add `color_never_outputs_plain_text_test`, asserting that styling helpers return no escape characters when color mode is `ColorNever`. Add `color_always_wraps_heading_test`, asserting that heading style contains an ANSI escape prefix when color mode is `ColorAlways`. Add `color_auto_is_plain_without_tty_helper_test` if `ColorAuto` is defined as plain in this phase.

18. Implement `src/scherzo/terminal/style.gleam` until the style tests pass.

19. Create `test/terminal_renderer_test.gleam`. Add `renders_session_header_test`, constructing a summary for `ABC-123` and asserting `chunks_to_string` output contains the issue identifier, title, workspace path, session id, and status.

20. In `test/terminal_renderer_test.gleam`, add `warns_when_replay_page_is_truncated_test`: pass an `EventPage` with `truncated: True` through the pretty page helper and assert output contains `older retained events were dropped`.

21. Add `groups_turn_and_assistant_delta_test`: feed `turn_start`, two `message_update` events with deltas `hello ` and `world`, and `turn_end`; assert transcript contains one turn heading, one assistant label, contiguous `hello world`, and a turn-ended line.

22. Add `renders_continued_turn_when_history_starts_mid_turn_test`: start with a `message_update` or tool update event whose payload has `turn: Some(4)` and no prior `turn_start`; assert output contains `turn 4 continued` before the assistant/tool content.

23. Add `suppresses_duplicate_cursor_test`: render an event with cursor `2`, then render another event with cursor `2` and different text; assert the second text is absent and `last_cursor` remains `2`.

24. Add `renders_tool_execution_block_test`: feed fake tool start/update/end events based on the captured real shape; assert output contains `tool`, `bash`, `gleam test`, `2 failures`, and `failed` or the equivalent captured values.

25. Add `renders_truncated_tool_output_test`: feed a tool event with a pre-truncated normalized `tool_output`; assert output contains the visible `… [truncated]` suffix and does not contain the omitted tail.

26. Add `renders_ui_request_and_unknown_event_test`: feed `extension_ui_request` with method `confirm` and an unknown pi type with redacted raw JSON; assert output shows `UI request`, `confirm`, and a fallback `event <type>` line without crashing.

27. Add `renders_token_summary_test`: feed a token stats event with input `1`, output `2`, cache values `0`, and total `3`; assert output contains `tokens` and `total=3`.

28. Add a replay pagination helper test in `test/terminal_renderer_test.gleam` or `test/ctl_test.gleam`: fake two pages where the first has 200 events and `next_cursor: 200`, and the second has one event and `next_cursor: 201`; assert pretty replay includes events from both pages and stops after the short page.

29. Implement `src/scherzo/terminal/render.gleam` until renderer tests pass with color disabled.

30. Refactor `src/scherzo/ctl.gleam` command types and parsing. Add parser tests in `test/ctl_test.gleam` for `attach ABC-123`, `attach --raw ABC-123`, `attach --json ABC-123`, `attach --raw --json ABC-123`, `attach --no-follow ABC-123`, `attach --since-cursor 40 ABC-123`, `attach --color=never ABC-123`, `events ABC-123`, `events --pretty ABC-123`, `events --json ABC-123`, mutually exclusive output flags other than the attach legacy alias, negative cursor, non-integer cursor, and invalid color mode.

31. Add the exact `ControlClient`, `Output`, and `run_with_deps` test seam in `src/scherzo/ctl.gleam` as specified in Plan of Work. Keep `main` and normal `run` behavior wired to real control client and terminal I/O.

32. Implement the paginated replay helper in `src/scherzo/ctl.gleam`. Add fake dependency tests for: a short single page, two advancing pages, a page that does not advance `next_cursor` and therefore stops safely, and a page with `truncated: True` that produces one warning in pretty output.

33. Add a CLI attach rendering test using fake dependencies. Fake `get_session` returns a summary, fake `get_events` returns replayed turn/tool events with `next_cursor: 2`, and fake `stream_events` invokes the callback with cursor `2` and cursor `3`. Assert default pretty output contains a header, assistant label, tool label, and the cursor `2` event only once.

34. Add raw/JSON follow duplicate tests. Assert `attach --raw ABC-123` and `attach --json ABC-123` both skip a streamed cursor `2` event already printed during replay and print cursor `3` exactly once.

35. Add raw/JSON compatibility tests. Assert `attach --raw --no-follow ABC-123` output equals `client.compact_event_line` for the fake event. Assert `attach --json --no-follow ABC-123` and `attach --raw --json --no-follow ABC-123` output parse as JSON with top-level `version`, `id`, `stream`, `session_id`, `cursor`, and `event`, and contain no ANSI escape. Assert `events --json ABC-123` still uses the protocol response envelope rather than pretty output.

36. Implement `src/scherzo/ctl.gleam` command execution until parser, fake dependency, raw compatibility, JSON compatibility, pagination, duplicate suppression, and existing tests pass.

37. Add or update a daemon/control integration test proving `FAKE_PI_TOOL=1` tool events survive from fake pi through EventHub and render as tool blocks through the control-client path.

38. Update `README.md` with sample transcripts for `scripts/scherzoctl attach`, `scripts/scherzoctl attach --raw`, `scripts/scherzoctl attach --json`, legacy `scripts/scherzoctl attach --raw --json`, `scripts/scherzoctl events`, `scripts/scherzoctl events --pretty`, and `scripts/scherzoctl events --json`. Keep examples concise and use fake issue identifiers such as `ABC-123`.

39. Run `direnv exec . gleam format`, `direnv exec . gleam format --check src test`, and `direnv exec . gleam test`. Record the final pass count in Progress.

40. Commit the renderer and CLI phase after validation passes. Suggested commit message: `Render Scherzo session attach output`.

## Testing and Falsifiability

The renderer is falsified if assistant deltas are printed as disconnected JSON fragments, if two assistant deltas cannot render as contiguous text, if tool events from the captured real pi shape cannot be distinguished from assistant text when normalized tool fields are present, if unknown events crash rendering, if truncated retained history is not visible in pretty replay, if replay that starts mid-turn lacks a continued-turn heading, if pretty replay silently omits later pages, if replay/follow duplicates cursor output in any attach mode, if raw output differs from `client.compact_event_line`, if attach JSON output differs from `protocol.stream_event_to_string`, if the legacy `attach --raw --json` alias stops producing JSON stream output, if `events --json` stops using the protocol response envelope, if large tool output is stored or rendered without truncation, or if color cannot be disabled for tests and logs.

Add deterministic tests in `test/session_event_test.gleam`, `test/control_protocol_test.gleam`, `test/pi_rpc_test.gleam`, `test/terminal_style_test.gleam`, `test/terminal_renderer_test.gleam`, `test/ctl_test.gleam`, and one daemon/control integration test that exercises tool events through fake pi and the EventHub. No automated test may require a real terminal, real Linear, or real pi. Real pi is used only for the discovery fixture or optional manual validation; once captured, the JSONL fixture makes the test suite deterministic.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Before implementation, tests that import `scherzo/terminal/render` should fail to compile. After implementation, all tests should pass. Record both the initial and final pass counts in Progress.

## Validation and Acceptance

Automated acceptance is required: run the format check and full test suite from the repository root and accept only if both pass.

Manual acceptance with the fake pi fixture is optional but recommended when a configured test Linear project and disposable test repository are available. From the repository root, create or edit a temporary workflow file under the repository, for example `.scherzo/tmp/attach-renderer-workflow.md`, using `examples/WORKFLOW.md` as the template. Set `pi.command` in that temporary workflow to `test/fixtures/fake_pi_rpc.sh`, keep `handoff.enabled: false`, set `workspace.root` to `.scherzo/workspaces`, and use a disposable `REPO_URL`. Then start daemon mode with tool events enabled:

    FAKE_PI_TOOL=1 LINEAR_API_KEY=lin_api_for_test_project REPO_URL=git@example.com:org/disposable-test-repo.git direnv exec . gleam run -- .scherzo/tmp/attach-renderer-workflow.md

In another terminal, use the logged control file or export it:

    export SCHERZO_CONTROL_FILE=.scherzo/workspaces/.scherzo-state/control.json
    scripts/scherzoctl ping
    scripts/scherzoctl ps
    scripts/scherzoctl attach --no-follow <session-id>

Accept fake-fixture pretty attach only if output shows a header with issue/workspace/session, turn boundaries, assistant text, a tool block matching the captured fixture shape, and final token/status information. Then run:

    scripts/scherzoctl attach --json --no-follow <session-id>
    scripts/scherzoctl attach --raw --json --no-follow <session-id>
    scripts/scherzoctl attach --raw --no-follow <session-id>
    scripts/scherzoctl events <session-id>
    scripts/scherzoctl events --pretty <session-id>
    scripts/scherzoctl events --json <session-id>

Accept only if JSON and raw modes still work, contain no ANSI escapes, and remain suitable for automation/debugging.

Manual acceptance with real pi is recommended before claiming tool rendering is fully validated. Use a disposable workspace and the real configured pi command. Prompt pi to perform one harmless tool action such as listing files or running a no-op test command. Run `scripts/scherzoctl attach --no-follow <session-id>` and accept only if the real tool action renders as a labeled tool block rather than only as fallback `event <type>` lines. If real pi is unavailable, record in Outcomes & Retrospective that tool rendering was validated only against captured fixtures.

## Rollout, Recovery, and Idempotence

This phase changes presentation and additive event fields only. It does not change scheduler state, worker behavior, control authentication, Linear interactions, or EventHub retention. If the pretty renderer is wrong, operators can use `attach --raw`, `attach --json`, the legacy `attach --raw --json`, default `events`, or `events --json` while the renderer is fixed.

The change is reversible at the CLI level because raw and JSON modes remain available. Additive event fields are backward-compatible: older JSON without the fields decodes with `None`, and newer JSON includes nullable fields that older consumers should ignore if they parse only known keys.

Renderer functions are pure and idempotent for the same input events and starting state. Attach clients track last cursor locally; reconnecting and replaying from an older cursor may print older retained events again, which is expected. During one attach invocation, duplicate cursor suppression prevents replay/live handoff duplicates in pretty, raw, and JSON modes. Pretty paginated replay may show more retained events than old compact `events`; compact `events` and `events --json` intentionally retain their existing one-page behavior.

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

If replay starts in the middle of a retained turn, pretty output includes a continued heading before the first event in that turn:

    ▶ turn 4 continued

Exact wording may differ, but the output must make turn, continued-turn, assistant, tool, UI, fallback events, truncation, and token events visually distinct.

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

Define the normalized tool text cap as a small private constant near the redaction/truncation helper, initially 4096 characters. The cap applies to `tool_input`, `tool_output`, and `tool_status` after redaction and before the value enters `PiUpdate` or `session.EventPayload`.

No new runtime dependency should be required. Use only existing JSON/session/control data and small ANSI helpers. If terminal detection for `ColorAuto` is added, implement it as a small isolated helper with tests; otherwise document and test that `ColorAuto` behaves as no color in this phase.
