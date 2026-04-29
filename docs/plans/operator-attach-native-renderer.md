# Implement the native line-oriented attach renderer

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries,
Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, an operator running `scripts/scherzoctl attach <session-id>` can read a live Scherzo session transcript without mistaking pi's internal lifecycle events for top-level Scherzo turns. The pretty transcript will group visible output under `Scherzo pass N`, make assistant text readable as a block, show tool input and output as separate blocks, preserve safe newlines, hide pi cycle noise by default, and expose diagnostic pi cycle and raw-event details only when the operator asks for `--verbose`.

This plan is the native-renderer plan split out after the renderer-reuse spike in `docs/spikes/pi-rendering-evaluation.md`. That spike found no stable, package-importable pi transcript renderer and found pi's interactive components too coupled to a TUI runtime for Scherzo's current plain terminal transcript. The chosen path is therefore to keep the renderer in Gleam, line-oriented, testable, and integrated with the existing `scherzoctl attach` and `events --pretty` flows.

At the time this plan was last reviewed, the native renderer implementation existed in the working tree. The implementation and this plan may remain uncommitted until the final commit point. This plan records the implementation intent, evidence, validation, and replayable steps so the work can be reviewed, reverted, or re-applied cleanly if the branch is rebuilt.

## Problem Framing and Constraints

The concrete operator problem is confusion during live supervision. In dogfood runs, the pretty attach transcript printed repeated lines such as `▶ turn 1 started` and `✓ turn 1 ended`. Those events came from pi's internal cycle lifecycle, but Scherzo's stored `EventPayload.turn` number represents the Scherzo runner pass. Reusing the word `turn` for both concepts made the operator think the worker was repeatedly starting and ending the same top-level turn.

The renderer must remain an ordinary line transcript for this phase. A full-screen terminal UI, scrollback pane, key handling, Markdown renderer, or Node helper is out of scope. `attach --raw`, `attach --json`, the legacy `attach --raw --json`, default compact `events`, and `events --json` are automation surfaces and must not change shape. Pretty rendering may improve, but raw and JSON output remain the fallback when an operator needs exact retained event data.

The implementation must also remain safe for terminals. Scherzo renders untrusted assistant messages, tool input, tool output, UI request text, raw pi excerpts, workspace paths, issue titles, and error messages. Normal newlines should be preserved in body text, but terminal control characters such as ESC, CSI, OSC, C0 controls, C1 controls, DEL, and carriage return must not be emitted as live controls. Scherzo-owned ANSI color from `src/scherzo/terminal/style.gleam` is the only ANSI allowed in pretty mode.

## Strategy Overview

Use the existing native Gleam renderer in `src/scherzo/terminal/render.gleam`. It is already pure enough to test with exact transcript assertions, already accepts retained `event.SessionEvent` values, and already feeds the `ctl.Output` seam used by `scherzoctl attach`. This is proportionate because the current problem is a confusing line transcript, not the absence of an interactive dashboard.

The core design is to separate three concepts in renderer state. `current_pass` is the most recently observed Scherzo pass number from `payload.turn`. `displayed_pass` is the pass heading actually emitted to the terminal. `pi_cycle` is a renderer-local counter for pi `turn_start` events. A hidden `turn_start` may update `current_pass` and increment `pi_cycle`, but it must not set `displayed_pass`; otherwise quiet output could lose the later `Scherzo pass N` heading for the first visible assistant, tool, UI, token, or error event.

Pretty output has two modes. Quiet output comes from `render.default_options(color_mode)` and hides pi cycle lifecycle events plus unknown raw payloads. Verbose output comes from `render.verbose_options(color_mode)` and enables lifecycle, raw unknown, and pi-cycle diagnostic lines. The CLI carries a boolean `verbose` only to pretty rendering; raw and JSON modes ignore it so their contracts stay stable.

Text rendering is block-oriented for assistant, UI request bodies, and tool input/output. `sanitize.text` remains the inline escaping function. `sanitize.block_lines` is the body function: it normalizes CRLF to LF, treats LF as layout, and escapes all other controls inside each line. For bounded tool display, `sanitize.bounded_body_lines` performs split, sanitize, and truncation in one pass while `src/scherzo/terminal/render.gleam` owns the display limits and user-facing truncation note. Assistant deltas keep streaming behavior by tracking whether an assistant line is open. Tool events track an active tool label and active subsection so repeated output updates for the same tool append under the existing `output` heading instead of repeating the full block heading. Hidden lifecycle records can be the only visible evidence of a Scherzo pass boundary, so observing a different pass must reset active tool context, and a following assistant event for the new pass must close the old assistant line before emitting the new pass heading.

## Alternatives Considered

Leaving the old renderer unchanged was rejected because it already misled an operator during dogfood. Training operators to ignore repeated `turn 1 started` lines does not solve the human-confidence problem.

Using pi's own interactive components was rejected for this phase by the spike in `docs/spikes/pi-rendering-evaluation.md`. `AssistantMessageComponent` can render lines only after theme initialization and emits pi-owned OSC/ANSI sequences plus fixed-width padding. `ToolExecutionComponent` requires a TUI object and mutable rendering lifecycle. Those properties do not fit a small Gleam CLI renderer that should work over ordinary terminals, SSH, and logs.

Adding a Node helper around pi, pi-tui, Ink, terminal-kit, chalk, marked, or a syntax highlighter was rejected for this phase because Scherzo does not currently have a Node package setup, importing pi by package name from the repository root fails, and packaging a supervised helper would add failure modes larger than the immediate line-transcript problem.

Building a full-screen TUI was rejected as disproportionate. That may become the right product later for multiple sessions, scrollback, filtering, and UI responses, but this plan only fixes the attach transcript.

## Risks and Countermeasures

The main correctness risk is suppressing the pass heading by confusing observed pass state with displayed heading state. The countermeasure is explicit state fields: `current_pass` and `displayed_pass`. Tests must include a quiet sequence that starts with hidden `turn_start` and then emits assistant output; the transcript must still contain `Scherzo pass 1`.

A related boundary-state risk is carrying active assistant or tool context across a hidden Scherzo pass boundary. If pass 1 ends with `tool bash` output and a hidden pass 2 `turn_start` arrives before another `tool bash` event, the renderer must still emit `Scherzo pass 2` and a fresh `tool bash` block rather than appending under pass 1's active output subsection. The countermeasure is to reset active tool state when a different pass is observed and to close an active assistant line before rendering a visible event for a different pass.

The main compatibility risk is changing raw or JSON automation output. The countermeasure is to route `--verbose` only into pretty rendering and to keep raw and JSON code paths in `src/scherzo/ctl.gleam` using `client.compact_event_line` and `protocol.stream_event_to_string` exactly as before. Tests in `test/ctl_attach_render_test.gleam` must continue to prove duplicate cursor suppression for raw and JSON attach.

The main terminal-safety risk is preserving normal newlines while accidentally emitting live controls embedded in assistant or tool text. The countermeasure is `sanitize.block_lines`, tests for ESC/C1/carriage return escaping, and use of block sanitization for every multiline body field.

The main output-size risk is flooding an operator terminal with tool output. The countermeasure is display-only truncation in `src/scherzo/terminal/render.gleam` using `default_max_body_lines = 40` and `default_max_body_line_chars = 200`, followed by the visible note `… [display truncated; use --json for retained raw event]`. This is separate from storage-time redaction and truncation.

The main UX risk is hiding pi lifecycle details that are useful while debugging. The countermeasure is the additive `--verbose` flag for pretty attach and pretty events. Verbose mode shows `pi cycle N started` and `pi cycle N ended` without reintroducing ambiguous `turn` wording.

## Progress

- [x] (2026-04-29 21:21Z) Confirmed the native renderer implementation work was present in the current checkout before authoring this split-out plan.
- [x] (2026-04-29 21:22Z) Re-read `docs/spikes/pi-rendering-evaluation.md` and confirmed the spike recommendation is native Gleam rendering.
- [x] (2026-04-29 21:23Z) Re-read `src/scherzo/terminal/render.gleam`, `src/scherzo/terminal/sanitize.gleam`, `src/scherzo/ctl.gleam`, `test/terminal_render_test.gleam`, `test/ctl_attach_render_test.gleam`, `test/ctl_test.gleam`, and the README attach documentation to fact-check the current implementation.
- [x] (2026-04-29 21:26Z) Ran `direnv exec . gleam test`; the suite passed during initial authoring. A later review of the current tree reports `289 passed, no failures`.
- [x] (2026-04-29 21:28Z) Authored this split-out native-renderer ExecPlan as `docs/plans/operator-attach-native-renderer.md`.
- [x] (2026-04-29 21:33Z) Re-reviewed the plan against the current working tree. `git status --short` and `jj status --ignore-working-copy` showed in-flight native-renderer and plan changes rather than a clean post-commit tree.
- [x] (2026-04-29 21:33Z) Ran `direnv exec . gleam format --check src test`, `direnv exec . gleam test`, and `direnv exec . gleam run -- ctl --help`; validation passed and the test suite reported `289 passed, no failures`.
- [x] (2026-04-29 21:34Z) Amended this plan to cover hidden pass-boundary state resets, `sanitize.bounded_body_lines`, current validation evidence, and portable sample paths.
- [x] (2026-04-29 21:40Z) Started a local fake control server and confirmed quiet `scripts/scherzoctl attach SMOKE-ATTACH-1` rendered `Scherzo pass 1`, assistant, tool, UI request, and token summary blocks without ambiguous turn lifecycle lines.
- [x] (2026-04-29 21:41Z) Ran verbose attach against the fake control server and confirmed `pi cycle 1 started`, `pi cycle 1 ended`, `pi cycle 2 started`, and `pi cycle 2 ended` diagnostics appear only with `--verbose`.
- [x] (2026-04-29 22:00Z) Started the real Scherzo daemon with `.scherzo/workflows/research.md`, attached to live session `LIV-11--576460751317-1`, and observed the renderer with real pi event traffic.
- [x] (2026-04-29 22:05Z) Discovered real pi can emit many structured tool input snapshots with the placeholder `[structured tool input; use --json for raw details]`; added renderer state and tests to collapse repeated structured placeholder input per tool label per Scherzo pass.
- [x] (2026-04-29 22:09Z) Re-ran `direnv exec . gleam test`; the suite reported `290 passed, no failures`.
- [x] (2026-04-29 22:10Z) Re-attached to the real daemon with the updated renderer and confirmed retained output had no ambiguous lifecycle lines and only one structured tool input placeholder for the retained Scherzo pass.
- [x] (2026-04-29 22:11Z) Stopped the real daemon with SIGTERM to the BEAM process; the control file and instance lock were removed.

## Surprises & Discoveries

- Observation: The current renderer can remain pure and line-oriented even with streaming assistant deltas.
  Evidence: `render.render_event` and `render.render_events` still return `#(RenderState, List(RenderChunk))`, and tests assert exact strings through `render.chunks_to_string` without starting a daemon.

- Observation: The active assistant line, active tool subsection, and repeated structured tool placeholders are separate state problems.
  Evidence: `RenderState` carries both `assistant_active` and `assistant_line_open` for assistant streaming, `active_tool_label` plus `active_tool_section` for repeated tool output updates, and `structured_tool_input_labels` so real pi snapshots do not print the same structured input placeholder repeatedly within one Scherzo pass.

- Observation: The CLI can expose verbose pretty output without affecting raw or JSON output.
  Evidence: `src/scherzo/ctl.gleam` carries `verbose: Bool` in `Command.Events` and `Command.Attach`, but raw and JSON modes still print compact lines or JSON stream envelopes; only pretty mode calls `pretty_options(color, verbose)`.

- Observation: Live attach smoke testing was not available in the authoring environment.
  Evidence: The previous implementation validation found `SCHERZO_CONTROL_FILE` unset. The fake-control tests in `test/ctl_attach_render_test.gleam` cover replay, follow, duplicate suppression, quiet lifecycle hiding, and verbose cycle diagnostics without needing a daemon.

- Observation: Hidden lifecycle events can advance from one Scherzo pass to another while the same tool label is still active.
  Evidence: `src/scherzo/terminal/render.gleam` resets active tool state when `current_pass` changes, and `test/terminal_render_test.gleam` contains `render_tool_label_resets_across_hidden_pass_boundary_test` to prove `tool bash` is reintroduced under `Scherzo pass 2`.

- Observation: The current review checkout contains in-flight implementation and plan edits rather than a clean post-implementation commit.
  Evidence: `git status --short` and `jj status --ignore-working-copy` showed `docs/plans/operator-attach-native-renderer.md`, `src/scherzo/terminal/render.gleam`, `src/scherzo/terminal/sanitize.gleam`, and `test/terminal_render_test.gleam` as changed. The final commit point must include those files unless the implementation is committed separately first.

- Observation: The first fake-control live smoke passed, but a real daemon attach exposed a separate noise source from real pi event shapes.
  Evidence: `scripts/scherzoctl attach --no-follow LIV-11--576460751317-1` against `.scherzo/workflows/research.md` initially printed many repeated `tool bash` blocks containing only `[structured tool input; use --json for raw details]`. A JSON page showed repeated `tool_execution_update` records whose only visible detail was the structured input placeholder.

- Observation: Collapsing structured tool input placeholders in the renderer is enough to improve retained real-daemon output without changing raw or JSON data.
  Evidence: After updating `src/scherzo/terminal/render.gleam`, the same retained real-daemon session rendered one placeholder block for `tool bash` in Scherzo pass 9, no label-only duplicate tool block, and no `turn N started`/`turn N ended` lines. The full suite reported `290 passed, no failures`.

## Decision Log

- Decision: Keep the implementation in the native Gleam renderer rather than adding pi or Node rendering.
  Rationale: The spike did not find a stable, package-importable transcript renderer, and pi's exported interactive components are coupled to theme/TUI runtime behavior. The native renderer is already integrated and testable.
  Date: 2026-04-29

- Decision: Use `Scherzo pass N` for visible top-level grouping.
  Rationale: The numeric `payload.turn` value in stored events represents Scherzo's runner pass, while pi also emits events named `turn_start` and `turn_end`. `Scherzo pass` avoids overloading the word `turn`.
  Date: 2026-04-29

- Decision: Hide pi `turn_start` and `turn_end` cycle events by default and show them only as `pi cycle` diagnostics in verbose pretty mode.
  Rationale: Quiet attach should emphasize assistant output, tools, UI requests, tokens, and errors. Pi cycle boundaries are useful for debugging but noisy and misleading in default output.
  Date: 2026-04-29

- Decision: Preserve ordinary newlines in body text through `sanitize.block_lines` rather than by weakening `sanitize.text`.
  Rationale: Inline labels still need simple one-line escaping, while body rendering needs newline layout. Splitting the functions prevents accidental terminal-control regressions in labels.
  Date: 2026-04-29

- Decision: Use display truncation for pretty tool blocks while preserving raw/JSON access to retained event details.
  Rationale: Operators need a bounded terminal transcript. Automation and debugging can use raw or JSON output for retained event data.
  Date: 2026-04-29

- Decision: Keep bounded body splitting and truncation in `src/scherzo/terminal/sanitize.gleam` as `sanitize.bounded_body_lines` while keeping display constants and the truncation note in `src/scherzo/terminal/render.gleam`.
  Rationale: Tool display needs to sanitize and truncate without slicing raw terminal controls incorrectly. Centralizing the bounded split in the sanitization module keeps that invariant near the escaping code while leaving renderer-specific limits out of the sanitizer API.
  Date: 2026-04-29

- Decision: Reset active tool context when observing a different Scherzo pass, even if the boundary event is hidden in quiet mode.
  Rationale: Repeated updates for the same tool should append within one pass, but the same tool name in a later pass must start a fresh block under a fresh `Scherzo pass N` heading.
  Date: 2026-04-29

- Decision: Document this native-renderer work in a separate plan even though the first implementation landed through the combined spike plan.
  Rationale: The spike and the native renderer have different purposes. Splitting the native plan makes the chosen path reviewable without re-reading the rejected pi/Node alternatives.
  Date: 2026-04-29

- Decision: Collapse repeated structured tool input placeholders per tool label per Scherzo pass in the pretty renderer.
  Rationale: Real pi event streams can include many repeated snapshots whose only retained input is `[structured tool input; use --json for raw details]`. Reprinting the placeholder dozens of times adds noise but no information. Raw and JSON modes still retain every event for debugging.
  Date: 2026-04-29

## Outcomes & Retrospective

The native renderer is implemented in the current tree. Quiet pretty output no longer shows repeated `turn 1 started` and `turn 1 ended` lines. It emits `Scherzo pass 1`, an `assistant` block, tool subsections, UI request lines, and pass-scoped token summaries. Verbose pretty output adds `pi cycle N started` and `pi cycle N ended` diagnostic lines.

The current validation evidence is:

    direnv exec . gleam format --check src test
    # exits 0
    direnv exec . gleam test
    290 passed, no failures
    direnv exec . gleam run -- ctl --help
    # output includes attach --verbose, events --pretty --verbose, and --verbose

A fake-control live attach smoke produced the target quiet transcript with `Scherzo pass 1`, assistant text, tool input/output, UI request text, and a token summary. A verbose fake-control attach added `pi cycle` diagnostics. A real daemon attach against `.scherzo/workflows/research.md` exposed repeated structured tool input placeholders; after the renderer fix, the retained real-daemon transcript collapsed those duplicates while still showing one placeholder and pass token summaries.

A representative quiet transcript shape is:

    Scherzo pass 1
    assistant
      I will run the tests and inspect the failure.
    tool bash
      input
        gleam test
      output
        289 passed, no failures
      status: success
    Scherzo pass 1 tokens: input=10 output=20 cache_read=3 cache_write=4 total=37

The main implementation lesson is that the native renderer was the right size for the current product need. The design improves readability without adding a second runtime, without taking a dependency on pi internals, and without changing raw or JSON contracts. A future full-screen TUI should be a separate plan with its own package, rendering, and rollback story.

## Context and Orientation

Scherzo is a Gleam project targeting Erlang. Source code lives under `src/scherzo/`, tests live under `test/`, documentation lives in `README.md` and `docs/`, and execution plans live under `docs/plans/`. The normal validation command from the repository root is `direnv exec . gleam test`.

The local operator CLI is implemented in `src/scherzo/ctl.gleam`. The command `attach <session-id>` fetches retained events from the control API, prints them, and by default follows live events through `ControlClient.stream_events`. The command `events <session-id>` replays retained events without following. Pretty rendering is used for default attach and for `events --pretty`. Raw output uses compact event lines from `scherzo/control/client.gleam`; JSON output uses protocol JSON from `scherzo/control/protocol.gleam`.

The native pretty renderer is implemented in `src/scherzo/terminal/render.gleam`. It defines `RenderChunk`, `ToolSection`, `RenderState`, `RenderOptions`, `initial_state`, `default_options`, `verbose_options`, `chunks_to_string`, `render_header`, `render_truncation_warning`, `render_event`, `render_events`, and `render_page`. `RenderChunk` has `Line(String)` and `Inline(String)` so streaming assistant deltas can append without forcing a newline after every event.

Terminal styling is implemented in `src/scherzo/terminal/style.gleam`. It owns Scherzo's ANSI styling for headings, dim text, success, warning, error, assistant labels, and tool labels. Renderer-owned ANSI styles are safe because they are added after untrusted text is sanitized.

Terminal sanitization is implemented in `src/scherzo/terminal/sanitize.gleam`. The function `text(value: String) -> String` escapes controls for inline text. The function `block_lines(value: String) -> List(String)` normalizes CRLF to LF, splits on LF, and escapes controls inside each returned line. The function `bounded_body_lines(value, max_lines, max_chars, truncation_note)` performs the same safe block splitting while limiting displayed line count and line width for pretty tool bodies.

Session event types are defined in `src/scherzo/session/event.gleam`. `EventPayload` includes `kind`, `name`, `turn`, `pi_type`, `message`, `request_id`, `method`, `tool_name`, `tool_input`, `tool_output`, `tool_status`, `tokens`, and `raw_json`. The field `turn` is retained for compatibility, but pretty rendering displays it as a Scherzo pass.

## Preconditions and Verified Facts

Before changing code under this plan, run these commands from the repository root:

    git status --short
    jj status --ignore-working-copy
    direnv exec . gleam test

As of the 2026-04-29 22:11Z review, `git status --short` and `jj status --ignore-working-copy` showed in-flight native-renderer and plan changes rather than a clean post-implementation commit. The changed files were `docs/plans/operator-attach-native-renderer.md`, `src/scherzo/terminal/render.gleam`, and `test/terminal_render_test.gleam`. `direnv exec . gleam test` passed and reported `290 passed, no failures`. Treat those files as the current implementation set unless they are committed before this plan is reused.

Verified current facts:

- `docs/spikes/pi-rendering-evaluation.md` exists and recommends proceeding with the native Gleam renderer.
- `src/scherzo/terminal/render.gleam` exposes `default_options(color_mode)` and `verbose_options(color_mode)`, and `RenderOptions` contains `color_mode`, `show_lifecycle`, `show_raw_unknown`, and `show_pi_cycles`.
- `src/scherzo/terminal/render.gleam` uses `current_pass`, `displayed_pass`, and `pi_cycle` in `RenderState`.
- `src/scherzo/terminal/render.gleam` renders `payload.name == "turn_start"` and `payload.name == "turn_end"` through pi-cycle helpers, not through user-visible `turn` lines.
- `src/scherzo/terminal/render.gleam` resets active tool context when an observed pass changes so same-named tools do not bleed across hidden pass boundaries.
- `src/scherzo/terminal/sanitize.gleam` exposes `block_lines` for body rendering, `bounded_body_lines` for bounded pretty body rendering, and leaves `text` available for inline text.
- `src/scherzo/ctl.gleam` parses `--verbose`, carries it through `Command.Events` and `Command.Attach`, and selects verbose render options only for pretty mode.
- `README.md` documents quiet attach, verbose attach, raw attach, JSON attach, `events --pretty`, and `events --pretty --verbose`.
- `test/terminal_render_test.gleam`, `test/ctl_attach_render_test.gleam`, and `test/ctl_test.gleam` cover the native renderer behavior.

If any of these facts become false in a later checkout, update this plan before using it as implementation guidance.

## Scope Boundaries

In scope: native pretty rendering in `src/scherzo/terminal/render.gleam`; block sanitization and bounded body splitting in `src/scherzo/terminal/sanitize.gleam`; pretty CLI verbosity wiring in `src/scherzo/ctl.gleam`; exact renderer tests; injected-control attach tests; CLI parser/help tests; and README documentation for attach and events pretty output.

Out of scope: pi or Node renderer dependencies; full-screen TUI; Markdown rendering; syntax highlighting; terminal width detection; interactive transcript navigation; answering UI requests from attach; changing EventHub retention; changing the control protocol JSON schema; changing raw or JSON output contracts; renaming protocol fields such as `turn` or `current_turn`; changing non-attach commands such as `stop-after-turn`; and durable transcript storage.

The UTF-8 raw JSON truncation fix is related substrate work but is not the main native renderer concern. In the current tree it is already implemented in `src/scherzo_redaction_ffi.erl` and covered by `test/session_redaction_test.gleam`. If applying this plan to an earlier checkout where that fix does not exist, apply that substrate fix first or keep it in a separate small plan.

## Milestones

Milestone 1 establishes the native renderer state model. At the end, `RenderState` separates observed Scherzo pass, displayed pass heading, pi-cycle count, assistant line state, active tool label, and active tool section. Tests prove hidden pi lifecycle events do not suppress a later `Scherzo pass N` heading and do not let active tool state bleed into a later Scherzo pass.

Milestone 2 implements quiet and verbose lifecycle behavior. At the end, quiet pretty output hides pi `turn_start` and `turn_end`, while verbose pretty output emits `pi cycle N started` and `pi cycle N ended`. No pretty output labels those pi lifecycle events as plain `turn` lines.

Milestone 3 implements safe block body rendering. At the end, assistant text and tool input/output preserve ordinary newlines, escape terminal controls, and keep adjacent assistant deltas on the same line when no newline occurs.

Milestone 4 improves tool, UI, token, and display-size rendering. At the end, tool events render with `input`, `output`, and `status` subsections; repeated output-only updates append under the current `output` section; long displayed output includes a truncation note; UI requests remain prominent; and token summaries name the Scherzo pass when known.

Milestone 5 wires CLI verbosity and documentation. At the end, `attach --verbose <session-id>` and `events --pretty --verbose <session-id>` use verbose pretty rendering, raw and JSON modes remain stable, help text documents `--verbose`, and `README.md` shows the quiet and verbose attach usage.

Milestone 6 validates the implementation. At the end, format checks pass, the full test suite passes, help output mentions verbose pretty usage, and either a live attach smoke or fake-control attach tests demonstrate quiet and verbose transcripts.

## Plan of Work

In `src/scherzo/terminal/render.gleam`, define `ToolSection` with variants `ToolInput` and `ToolOutput`. Expand `RenderState` so it carries `last_cursor`, `current_pass`, `displayed_pass`, `pi_cycle`, `assistant_active`, `assistant_line_open`, `active_tool_label`, `active_tool_section`, and `structured_tool_input_labels`. Keep `last_cursor` as the replay/follow duplicate-suppression cursor. Do not reuse a single field for both observed pass and displayed heading. When an observed pass changes, clear `active_tool_label`, `active_tool_section`, and `structured_tool_input_labels`; when a visible assistant event belongs to a different pass than the displayed pass, close the previous assistant line before rendering the new heading and label.

In the same file, extend `RenderOptions` to include `show_pi_cycles` alongside `color_mode`, `show_lifecycle`, and `show_raw_unknown`. Keep `default_options(color_mode)` as quiet operator output with diagnostics disabled. Add `verbose_options(color_mode)` with lifecycle, raw unknown, and pi-cycle diagnostics enabled.

Change event dispatch so `payload.name == "turn_start"` calls `render_pi_cycle_start` and `payload.name == "turn_end"` calls `render_pi_cycle_end`. `render_pi_cycle_start` must increment `pi_cycle` and observe `payload.turn`. In quiet mode it returns no chunks. In verbose mode it closes active assistant/tool context, ensures the pass heading if needed, and prints `pi cycle N started`. `render_pi_cycle_end` observes the pass and in verbose mode prints `pi cycle N ended` only if at least one cycle start has been seen.

Add `ensure_pass_heading`. It should choose `payload.turn` when present, otherwise `state.current_pass`. If it knows a pass and `state.displayed_pass != Some(pass)`, it emits `Scherzo pass N`, sets `current_pass`, and sets `displayed_pass`. If the heading was already displayed, it emits nothing. Hidden pi lifecycle events may call `observe_pass`, but must not set `displayed_pass`. `observe_pass` should delegate to an `observe_visible_pass`-style helper that resets active tool context whenever `current_pass` changes.

In `src/scherzo/terminal/sanitize.gleam`, add `block_lines(value: String) -> List(String)`. It should replace CRLF with LF, split on LF, and run the same control escaping used by `text` on each non-newline codepoint. Newline is layout, not a control picture, only in `block_lines`. A lone carriage return remains escaped as `␍`. Also add `bounded_body_lines(value: String, max_lines: Int, max_chars: Int, truncation_note: String) -> #(List(String), Bool)` so tool display can sanitize and bound text without slicing live control bytes.

Update assistant rendering in `src/scherzo/terminal/render.gleam`. The visible label is `assistant`, without a colon. The renderer should use `sanitize.block_lines` for message deltas, emit `Inline("  " <> first_fragment)` for a new body line, emit `Line("")` when a newline completes a line, and track `assistant_line_open` so adjacent deltas such as `"Hello "` then `"world"` render on one line. When a non-assistant event arrives, close an open assistant line by emitting a blank `Line("")` chunk.

Update tool rendering in `src/scherzo/terminal/render.gleam`. The label is `tool <tool_name>` when `payload.tool_name` exists, otherwise `tool <payload.name>`. Render input and output as subsection headings followed by indented body lines. For example:

    tool bash
      input
        gleam test
      output
        289 passed, no failures
      status: success

If repeated update events for the same tool only add output within the same Scherzo pass, do not repeat `tool bash` or `output`; append new output body lines under the existing section. A new tool input, different tool label, status field, end event, non-tool event, or observed pass change closes or resets the active section.

Add display truncation through `sanitize.bounded_body_lines` and renderer-owned constants in `src/scherzo/terminal/render.gleam`. Use `default_max_body_lines = 40`, `default_max_body_line_chars = 200`, and the visible note `… [display truncated; use --json for retained raw event]`. Apply these helpers to displayed tool input and output bodies. If a line is longer than the character limit, append the note on that displayed line; if more lines exist than the line limit, append the note as its own displayed line. Do not apply these limits to raw or JSON modes.

Update UI request rendering so default output says `UI request waiting: <method> #<request-id>` and includes an indented body block when `payload.message` exists. Keep UI responses dim. Update token summaries so a known pass renders `Scherzo pass N tokens: input=... output=... cache_read=... cache_write=... total=...`; when no pass is known, keep neutral `tokens: ...` wording.

In `src/scherzo/ctl.gleam`, add `verbose: Bool` to `Flags`, parse `--verbose`, and carry it into `Command.Events` and `Command.Attach`. Add a helper such as `pretty_options(color, verbose)` that chooses `render.verbose_options(color)` only when pretty output is active and `verbose` is true. Raw and JSON branches should ignore `verbose` and keep their existing output functions.

Update `README.md` in the `Local control API and scherzoctl` section. Replace turn-oriented output examples with `Scherzo pass` examples, mention `--verbose`, and state that raw and JSON modes are unchanged and not affected by `--verbose`.

## Concrete Steps

1. From the repository root, run:

       git status --short
       jj status --ignore-working-copy
       direnv exec . gleam test

   Expect a clean or consciously documented working copy and a passing baseline. In the current implementation tree, the suite reports `290 passed, no failures`.

2. In `src/scherzo/terminal/render.gleam`, update `RenderState` and `RenderOptions` to the native renderer shape described above. Keep the public entry points `initial_state`, `default_options`, `verbose_options`, `chunks_to_string`, `render_header`, `render_truncation_warning`, `render_event`, `render_events`, and `render_page`.

3. In `test/terminal_render_test.gleam`, add or update tests for `default_options` quiet rendering and `verbose_options` diagnostic rendering. Include a default event sequence with hidden `turn_start` followed by assistant output and assert `Scherzo pass 1` appears.

4. In `src/scherzo/terminal/render.gleam`, replace visible `turn` lifecycle rendering with quiet hidden pi cycle handling and verbose `pi cycle` handling. Run `direnv exec . gleam test` and expect terminology tests to pass.

5. In `src/scherzo/terminal/sanitize.gleam`, add `block_lines` and `bounded_body_lines`. In `test/terminal_render_test.gleam`, assert `sanitize.block_lines("a\nb") == ["a", "b"]`, `sanitize.block_lines("a\r\nb") == ["a", "b"]`, and an input containing ESC/C1 controls does not return live control bytes. Also assert `sanitize.bounded_body_lines("1\n2\n3", 2, 200, "…") == #(["1", "2"], True)` and `sanitize.bounded_body_lines("abcd", 40, 3, "…") == #(["abc …"], False)`.

6. Update assistant rendering in `src/scherzo/terminal/render.gleam`. Add exact transcript tests for a multiline single delta, a newline split across two deltas, adjacent deltas with no newline, terminal-control escaping inside assistant content, and an assistant event for a new Scherzo pass that follows an open assistant line from the prior pass.

7. Update tool rendering in `src/scherzo/terminal/render.gleam`. Add exact transcript tests for single-line input/output, multiline output, repeated output-only updates within one pass, repeated structured input placeholder updates within one pass, status/end closure, display truncation by line count and by line width, and same-named tool output after a hidden pass boundary. The hidden-boundary test should include pass 1 `tool bash` output, a quiet `turn_start` for pass 2, and pass 2 `tool bash` output; the expected transcript must repeat both `Scherzo pass 2` and `tool bash`.

8. Update UI request and token rendering in `src/scherzo/terminal/render.gleam`. Add tests for `UI request waiting: confirm #ui-1` with a multiline body and `Scherzo pass 1 tokens: ...`.

9. In `src/scherzo/ctl.gleam`, parse `--verbose`, carry it through `Command.Events` and `Command.Attach`, and select `render.verbose_options` only for pretty mode. Update `test/ctl_test.gleam` to assert parsing for `attach --verbose ABC-1` and `events --pretty --verbose ABC-1`, and to assert usage text includes the new flag.

10. In `test/ctl_attach_render_test.gleam`, add or update fake-control tests so default pretty attach hides pi lifecycle lines, verbose pretty attach shows pi cycle lines, and raw/JSON follow still skip replayed duplicate cursors without emitting pretty text.

11. Update the `README.md` attach documentation and examples.

12. Run final validation from the repository root:

       direnv exec . gleam format --check src test
       direnv exec . gleam test
       direnv exec . gleam run -- ctl --help

   Expect the format check to pass, the full test suite to pass, and help output to mention `attach --verbose`, `events --pretty --verbose`, and `--verbose` in the options list.

13. If a live control file is available, run:

       export SCHERZO_CONTROL_FILE=<logged-control-file>
       scripts/scherzoctl attach --no-follow <session-id>
       scripts/scherzoctl attach --no-follow --verbose <session-id>

   Expect quiet output to contain `Scherzo pass` and no repeated `turn 1 started` lines. Expect verbose output to contain `pi cycle` lines. If no live control file is available, record that fact and rely on fake-control tests.

14. Update this plan's Progress, Surprises & Discoveries, and Outcomes & Retrospective. Commit the plan and implementation as one or more logical commits. If the implementation is already committed before this plan is reused, commit only this split-out plan; otherwise include the changed renderer, sanitizer, tests, documentation, and plan files in the relevant logical commit(s).

## Testing and Falsifiability

The renderer state model is falsifiable with exact transcript tests. A quiet sequence containing `turn_start` followed by assistant output must include `Scherzo pass 1`; a quiet sequence with repeated pi `turn_start` and `turn_end` must not include `turn 1 started`, `turn 1 ended`, or `pi cycle`; a verbose render of the same sequence must include `pi cycle 1 started`, `pi cycle 1 ended`, and `pi cycle 2 started`. A quiet hidden pass boundary between same-named tool events must render a new `Scherzo pass N` heading and a fresh tool label instead of appending under the previous pass.

Block sanitization is falsifiable with direct function tests. `sanitize.block_lines("a\nb")` must return two lines, CRLF must normalize to LF, and ESC/C1/control input must not return live control bytes. `sanitize.bounded_body_lines("1\n2\n3", 2, 200, "…")` must return `#(["1", "2"], True)`, and `sanitize.bounded_body_lines("abcd", 40, 3, "…")` must return `#(["abc …"], False)`. `sanitize.text` must continue to render newline as a visible control picture for inline use.

Assistant rendering is falsifiable with exact transcripts. A single delta `"first\nsecond"` must render as two indented body lines. Deltas `"first\n"` followed by `"second"` must also render as two indented body lines. Deltas `"Hello "` followed by `"world"` must render as `Hello world` on one line under the `assistant` label. If a later assistant event belongs to a different Scherzo pass, the prior open assistant line must close before the new pass heading and the new `assistant` label appear.

Tool rendering is falsifiable with exact transcripts. A tool with input, output, and status must render separate subsections. Repeated output updates for the same tool within one pass must not repeat the tool label or `output` heading. Repeated structured input placeholder snapshots for the same tool label within one Scherzo pass must render the placeholder once and suppress duplicate placeholder-only updates. The same tool name in a later pass after a hidden lifecycle boundary must repeat the `tool <name>` label under the new pass heading. Long output must include the visible display truncation note for both line-count truncation and line-width truncation. Terminal controls inside tool input/output must be escaped.

CLI behavior is falsifiable with parser and injected-client tests. `ctl.parse(["attach", "--verbose", "ABC-1"])` must return an `Attach` command with pretty mode and `verbose: True`. `ctl.parse(["events", "--pretty", "--verbose", "ABC-1"])` must return an `Events` command with pretty mode and `verbose: True`. `attach --json --verbose` must still use JSON output, not pretty output.

Raw and JSON compatibility is falsifiable with attach tests. Replayed duplicate cursors must still be skipped during follow for raw and JSON modes, and those modes must continue to emit compact event lines or stream JSON envelopes rather than pretty text.

The full plan is disproved if default pretty attach output still contains repeated `turn 1 started` or `turn 1 ended` lines, if verbose pi lifecycle output uses the ambiguous word `turn` instead of `pi cycle`, if terminal controls from untrusted payloads reach the transcript, if raw/JSON output changes shape, or if the implementation requires a Node/pi renderer dependency.

## Validation and Acceptance

Acceptance is operator-visible. Given retained events containing pi lifecycle records, assistant message deltas, a tool execution, a UI request, token totals, and a worker exit, this command:

    scripts/scherzoctl attach --no-follow <session-id>

should produce output shaped like:

    ABC-123 Fix flaky tests
    workspace: <workspace-path>
    session: ABC-123-42-1
    status: running

    Scherzo pass 1
    assistant
      I will run the tests and inspect the failure.
    tool bash
      input
        gleam test
      output
        289 passed, no failures
      status: success
    UI request waiting: confirm #ui-1
      approve?
    Scherzo pass 1 tokens: input=10 output=20 cache_read=3 cache_write=4 total=37

The default transcript must not show `turn 1 started`, `turn 1 ended`, or `pi cycle` lines.

This command:

    scripts/scherzoctl attach --no-follow --verbose <session-id>

should include diagnostic lines such as:

    Scherzo pass 1
    pi cycle 1 started
    pi cycle 1 ended
    pi cycle 2 started
    assistant
      ...
    pi cycle 2 ended

These commands must preserve their automation contracts:

    scripts/scherzoctl attach --raw --no-follow <session-id>
    scripts/scherzoctl attach --json --no-follow <session-id>
    scripts/scherzoctl events --json <session-id>

Validation from the repository root must pass:

    direnv exec . gleam format --check src test
    direnv exec . gleam test
    direnv exec . gleam run -- ctl --help

In the current tree, `direnv exec . gleam test` reports `290 passed, no failures`.

## Rollout, Recovery, and Idempotence

This change is local to pretty CLI rendering and documentation. It does not change worker scheduling, EventHub retention, Linear state transitions, session JSON schema, raw event storage, or the control protocol. Operators can immediately fall back to `scripts/scherzoctl attach --raw <session-id>` or `scripts/scherzoctl attach --json <session-id>` if pretty output is confusing.

The `--verbose` flag is additive. Existing pretty invocations become more readable but do not require new flags. Existing raw and JSON invocations are unaffected. If the native renderer regresses, revert the renderer/CLI commit while keeping the spike note and this plan; the recovery path remains raw/JSON attach.

All plan steps are idempotent at the source level. Re-running tests is safe. Re-running `attach --no-follow` is safe because it only reads retained events. Re-running live `attach` follows from the selected cursor and does not mutate session state.

## Artifacts and Notes

The spike note that selected the native path is `docs/spikes/pi-rendering-evaluation.md`.

The old confusing quiet transcript shape was:

    ▶ turn 1 started
    ✓ turn 1 ended
    ▶ turn 1 started
    assistant:
      ...
    ✓ turn 1 ended

The target quiet transcript shape is:

    Scherzo pass 1
    assistant
      ...

The target verbose transcript shape is:

    Scherzo pass 1
    pi cycle 1 started
    pi cycle 1 ended
    pi cycle 2 started
    assistant
      ...
    pi cycle 2 ended

The current final validation evidence is:

    direnv exec . gleam format --check src test
    # exits 0
    direnv exec . gleam test
    290 passed, no failures
    direnv exec . gleam run -- ctl --help
    # output includes attach --verbose, events --pretty --verbose, and --verbose

## Interfaces and Dependencies

No new package dependency is required.

In `src/scherzo/terminal/sanitize.gleam`, keep:

    pub fn text(value: String) -> String
    pub fn block_lines(value: String) -> List(String)
    pub fn bounded_body_lines(value: String, max_lines: Int, max_chars: Int, truncation_note: String) -> #(List(String), Bool)

In `src/scherzo/terminal/render.gleam`, keep:

    pub type RenderChunk {
      Line(String)
      Inline(String)
    }

    pub type ToolSection {
      ToolInput
      ToolOutput
    }

    pub type RenderState {
      RenderState(
        last_cursor: Int,
        current_pass: Option(Int),
        displayed_pass: Option(Int),
        pi_cycle: Int,
        assistant_active: Bool,
        assistant_line_open: Bool,
        active_tool_label: Option(String),
        active_tool_section: Option(ToolSection),
        structured_tool_input_labels: List(String),
      )
    }

    pub type RenderOptions {
      RenderOptions(
        color_mode: style.ColorMode,
        show_lifecycle: Bool,
        show_raw_unknown: Bool,
        show_pi_cycles: Bool,
      )
    }

    pub fn initial_state(since_cursor: Int) -> RenderState
    pub fn default_options(color_mode: style.ColorMode) -> RenderOptions
    pub fn verbose_options(color_mode: style.ColorMode) -> RenderOptions
    pub fn chunks_to_string(chunks: List(RenderChunk)) -> String
    pub fn render_header(summary: event.SessionSummary, options: RenderOptions) -> List(RenderChunk)
    pub fn render_truncation_warning(options: RenderOptions) -> List(RenderChunk)
    pub fn render_event(state: RenderState, stored_event: event.SessionEvent, options: RenderOptions) -> #(RenderState, List(RenderChunk))
    pub fn render_events(state: RenderState, events: List(event.SessionEvent), options: RenderOptions) -> #(RenderState, List(RenderChunk))
    pub fn render_page(summary: event.SessionSummary, page: event.EventPage, options: RenderOptions) -> List(RenderChunk)

In `src/scherzo/ctl.gleam`, keep `verbose: Bool` on the pretty-capable command constructors:

    Events(
      control_file: Option(String),
      mode: OutputMode,
      color: style.ColorMode,
      since_cursor: Int,
      verbose: Bool,
      session_id: String,
    )

    Attach(
      control_file: Option(String),
      mode: OutputMode,
      color: style.ColorMode,
      follow: FollowMode,
      since_cursor: Int,
      verbose: Bool,
      session_id: String,
    )

The only external command assumption is the existing project workflow for validation:

    direnv exec . gleam test

## Review Revision Notes

On 2026-04-29, a plan review tightened this ExecPlan rather than changing the implementation direction. The review updated stale validation evidence from 288 to 289 passing tests, removed the portable-plan violation caused by an absolute workspace path in sample output, documented the current in-flight working-copy state, added explicit hidden pass-boundary state-reset requirements, and aligned the sanitizer interface with the current `sanitize.bounded_body_lines` implementation. These revisions make the plan safer to reapply from a fresh checkout and reduce the chance that repeated tool output bleeds across Scherzo pass boundaries.
