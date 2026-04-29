# Improve the operator attach UI for Scherzo sessions

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, an operator running `scherzoctl attach <session-id>` can follow a live worker without being misled by repeated pi lifecycle events or forced to read raw JSON. The attach transcript will clearly distinguish a Scherzo pass from pi's internal cycles, make assistant output the primary object on screen, show tool input and output as readable blocks, and preserve raw and JSON modes for debugging and automation.

The work starts with an explicit rendering spike. The spike evaluates whether Scherzo can reuse pi's own rendering, a thin TypeScript or Node helper around pi APIs, `@mariozechner/pi-tui` primitives, or another third-party terminal rendering library. Unless that spike proves a stable, portable, low-risk drop-in path, this plan continues by refining the existing native Gleam renderer in `src/scherzo/terminal/render.gleam`.

## Problem Framing and Constraints

The current pretty attach output is useful enough to prove the EventHub and control API, but it is not good enough for dogfooding. During a real Linear-board run, attach showed repeated lines such as `▶ turn 1 started` and `✓ turn 1 ended`. Those lines are pi lifecycle events, but the label says only `turn`, which makes the operator think Scherzo is repeatedly starting and ending the same top-level worker turn. The operator needs to understand the run quickly: what prompt pass Scherzo is in, what pi is saying, what tools are running, whether a UI request is blocking, and whether the worker has finished.

The renderer must stay line-oriented for this phase. A full-screen terminal UI, scrolling panes, key handling, and interactive transcript navigation are attractive, but they are larger than the immediate dogfood problem. `scherzoctl attach` must continue to work over ordinary terminals, SSH, and logs. `attach --raw`, `attach --json`, and `events --json` must remain stable automation surfaces. No Linear command-comment transport is added here, and no Scherzo-to-Linear result posting is added here; those are covered by separate plans.

A reliability issue observed during attach also belongs in this plan because a UI that crashes is not usable. Redacted raw pi JSON is truncated in `src/scherzo_redaction_ffi.erl`; if truncation cuts a multibyte UTF-8 character, later JSON encoding in `src/scherzo/session/json.gleam` can crash with an Erlang `unexpected_end` exception. This plan includes a small substrate fix before changing presentation.

## Strategy Overview

First, perform a bounded rendering spike and record the evidence in a checked-in spike note. The spike does not change production behavior. It answers a concrete question: is there a stable pi or third-party renderer that can render Scherzo's retained event stream in a plain line-oriented CLI without turning Scherzo into a Node application or depending on a local, non-portable package path? At the end of the spike, the implementer must update `docs/spikes/pi-rendering-evaluation.md`, update this plan's Decision Log with the recommendation, and surface a short spike summary before editing production renderer code. If the answer is yes, the implementer updates this plan before coding the main renderer path. If the answer is no or unclear, the implementer proceeds with the native Gleam path described below after the spike summary checkpoint is recorded.

Second, fix the raw JSON truncation crash. This is small, testable, and independent of layout. The fix must make every retained `RedactedRawJson.value` valid UTF-8 after truncation so `session_json.event_to_string` and control replay cannot crash while rendering unknown events.

Third, rename the concepts shown by the pretty renderer. In Scherzo, the `turn` field on `session.EventPayload` currently means the Scherzo runner pass through `runner.loop_turns`, not a unique pi internal lifecycle cycle. The pretty transcript should therefore label that grouping as `Scherzo pass N`. Pi `turn_start` and `turn_end` events should be hidden by default. When verbose output is requested, those pi events should be labeled as `pi cycle N started` and `pi cycle N ended`, not as Scherzo turns. The renderer must keep "which Scherzo pass are we currently seeing?" separate from "has the Scherzo pass heading already been printed?" Hidden pi lifecycle events may update the current pass and pi-cycle counter, but they must not mark a pass heading as displayed in quiet mode; otherwise a quiet transcript that starts with `turn_start` can suppress the later `Scherzo pass N` heading before assistant or tool output.

Fourth, improve body rendering while keeping the existing EventHub and control protocol. Assistant text and tool output should preserve ordinary newlines, split into indented lines, and still escape terminal control characters. Tool events should render as compact blocks with separate `input`, `output`, and `status` sections. Unknown or low-value lifecycle events stay hidden by default and visible in verbose mode.

Finally, wire a small `--verbose` option through `scherzoctl` for pretty `attach` and `events --pretty`, update tests and docs, and validate with both synthetic tests and a short live fake-control transcript.

## Alternatives Considered

The simplest alternative is to leave the renderer as-is and train operators to ignore pi lifecycle noise. That is not acceptable because the first real dogfood run already showed that the output is misleading at the exact moment an operator needs confidence.

A second alternative is to immediately replace the renderer with pi's interactive UI. Pi has high-quality interactive rendering and exports component classes such as assistant, user, and tool execution components from its package entry point. The early authoring investigation found no obvious documented standalone transcript renderer that accepts Scherzo's stored events and emits a replayable line transcript. Those components may also require pi's TUI runtime, tool definitions, and Node packaging. This is promising enough to spike, but not proven enough to prescribe as the implementation.

A third alternative is to add a Node helper process that reads Scherzo events as JSON and renders them using pi or a terminal library such as Ink, terminal-kit, chalk, marked, or cli-highlight. That could improve Markdown and syntax rendering, but it adds process supervision, dependency packaging, failure modes, and snapshot-test complexity to a Gleam CLI. The spike may recommend this only if the quality jump is large and the helper can fail closed to the native renderer.

A fourth alternative is to build a full-screen TUI now. That would eventually help with multiple sessions, scrollback, filtering, and UI responses, but it is disproportionate for the current need. The current problem is a confusing line transcript, not the absence of a dashboard.

A fifth alternative is to keep raw JSON as the primary interface and ask operators to pipe it through `jq`. That preserves automation but fails the human operator goal.

## Risks and Countermeasures

The main product risk is spending too much time chasing pi or third-party rendering. Countermeasure: the spike has a one-day timebox, a written scorecard, and a default decision to continue with the native renderer unless a portable drop-in path is demonstrated.

The main compatibility risk is breaking scripts that use raw or JSON attach. Countermeasure: `attach --raw`, `attach --json`, the legacy `attach --raw --json`, default compact `events`, and `events --json` are not reformatted. New presentation applies only to pretty output.

The main terminology risk is replacing one confusing label with another. Countermeasure: tests must assert that default pretty output says `Scherzo pass 1` and does not contain `turn 1 started` for pi lifecycle events. Verbose output must say `pi cycle`, not `turn`, for pi lifecycle events.

The main renderer-state risk is hiding pi lifecycle events while accidentally treating them as visible pass headings. Countermeasure: `RenderState` must track the observed Scherzo pass separately from the pass heading that has actually been emitted. A hidden `turn_start` may set `current_pass` and increment `pi_cycle`, but it must not set `displayed_pass`; the next visible assistant, tool, UI, token, or error event for that pass must still print `Scherzo pass N`.

The main terminal-safety risk is preserving newlines while allowing terminal escape injection. Countermeasure: add block-oriented sanitization that treats newline as layout but still escapes C0, C1, DEL, ESC, OSC, CSI, carriage return, and other control characters inside each rendered line. Renderer-owned ANSI color remains the only ANSI emitted in pretty mode.

The main output-size risk is flooding the terminal with tool output. Countermeasure: tool text is already redacted and capped before storage in `src/scherzo/agent/runner.gleam`; the renderer should additionally cap displayed tool output by lines and line width, with a visible truncation note. Raw and JSON modes remain available for full retained details.

The main crash risk is invalid UTF-8 in retained raw JSON after byte truncation. Countermeasure: fix truncation in `src/scherzo_redaction_ffi.erl` to return only a valid UTF-8 prefix and add a regression test with a multibyte payload whose encoded JSON exceeds `redaction.max_raw_json_bytes`.

The main implementation risk is creating a broad rendering abstraction before the requirements are stable. Countermeasure: keep the production change in the existing `src/scherzo/terminal/` modules unless the spike proves otherwise. Do not add a new package manager, full TUI framework, or renderer plugin system in this phase.

## Progress

- [x] (2026-04-29 20:51Z) Stopped the live dogfood daemon before authoring this plan and verified no `gleam run -- .scherzo/workflows/research.md` process remained.
- [x] (2026-04-29 20:51Z) Read the existing attach renderer implementation in `src/scherzo/terminal/render.gleam` and CLI integration in `src/scherzo/ctl.gleam`.
- [x] (2026-04-29 20:51Z) Reviewed the existing `docs/plans/session-eventhub.md`, `docs/plans/terminal-attach-renderer.md`, and `docs/plans/linear-session-results.md` so this plan builds on current checked-in work rather than duplicating it.
- [x] (2026-04-29 20:51Z) Inspected pi package documentation and exports available in the authoring environment; pi exposes RPC/JSON event streams and interactive components, but no obvious documented drop-in transcript renderer was found.
- [x] (2026-04-29 20:51Z) Ran `direnv exec . gleam test`; the baseline reports `278 passed, no failures`.
- [x] (2026-04-29 20:58Z) Reviewed this ExecPlan against the current tree and tightened renderer state, precondition, CLI documentation, and streaming/truncation instructions before implementation.
- [x] (2026-04-29 21:02Z) Confirmed implementation baseline: working copy had only this added plan document, `jj status --ignore-working-copy` matched that doc-only state, and `direnv exec . gleam test` reported `278 passed, no failures`.
- [x] (2026-04-29 21:06Z) Performed and recorded the renderer reuse spike in `docs/spikes/pi-rendering-evaluation.md`; the spike recommends continuing with the native Gleam renderer.
- [ ] Fix UTF-8-safe raw JSON truncation and add a regression test.
- [ ] Rename pretty grouping from turn-oriented output to Scherzo-pass output and hide pi cycle events by default.
- [ ] Preserve safe newlines in assistant and tool body output.
- [ ] Improve tool block rendering and display truncation.
- [ ] Add `--verbose` pretty output and documentation.
- [ ] Validate the improved attach UI with tests and a short live or fake-control transcript.

## Surprises & Discoveries

- Observation: The current native renderer already has a pure testable shape: `render.render_event`, `render.render_events`, `render.render_page`, `RenderState`, `RenderOptions`, and output chunks. This makes iterative UI fixes cheaper than replacing the whole path.
  Evidence: `test/terminal_render_test.gleam` asserts exact transcripts without starting a daemon, and `test/ctl_attach_render_test.gleam` injects a fake `ctl.ControlClient` and fake output sink.

- Observation: `EventPayload.turn` is assigned by `src/scherzo/agent/runner.gleam` from the Scherzo runner loop pass, while event names such as `turn_start` and `turn_end` come from pi's internal lifecycle.
  Evidence: `runner.update_from_record(record, turn, secrets)` sets `PiUpdate.turn: Some(turn)` for every pi record in the current Scherzo pass, and `daemon.update_payload` copies that value into `EventPayload.turn`.

- Observation: A quiet renderer that hides `turn_start` must not use the same state bit to mean both "current pass is known" and "pass heading was printed".
  Evidence: The current `ensure_turn_heading` in `src/scherzo/terminal/render.gleam` emits a heading only when `state.current_turn` is `None`; if a future hidden `turn_start` set that field without emitting chunks, later assistant output would lose the required `Scherzo pass N` heading.

- Observation: The installed pi package exports useful interactive components and `RpcClient`, but package-level exports do not obviously include a standalone function that replays arbitrary JSON events into a plain transcript.
  Evidence: The package entry point exports classes such as `AssistantMessageComponent`, `ToolExecutionComponent`, `UserMessageComponent`, `renderDiff`, `Theme`, `RpcClient`, and `runRpcMode`; the authoring inspection did not find an exported transcript renderer.

- Observation: The implementation spike confirmed that importing `@mariozechner/pi-coding-agent` by package name from this repository fails, while importing through the installed CLI package path is machine-local and not suitable for a checked-in helper.
  Evidence: The Node probe from the repository root reported `Cannot find module '@mariozechner/pi-coding-agent/package.json'`; the installed CLI package could be inspected only by resolving the local `pi` executable.

- Observation: Pi interactive assistant rendering can return lines after theme initialization, but those lines include pi-owned terminal controls and fixed-width padding; tool rendering requires a TUI object.
  Evidence: The dynamic import probe recorded `AssistantMessageComponent.render(80)` output containing OSC prompt markers, and `ToolExecutionComponent.markExecutionStarted()` failed without a TUI object because it tried to call `requestRender`.

## Decision Log

- Decision: Start with a renderer-reuse spike, but default to improving the native Gleam renderer unless the spike proves a stable drop-in alternative.
  Rationale: Pi rendering might save effort and improve output quality, but unproven Node packaging or TUI coupling would add risk. The current native renderer is already integrated and tested.
  Date: 2026-04-29

- Decision: Rename the top-level pretty grouping to `Scherzo pass` rather than `turn`.
  Rationale: In stored session events, the numeric field currently represents Scherzo's loop pass. Pi also emits events named `turn_start` and `turn_end`, so using the same word for both concepts misleads operators.
  Date: 2026-04-29

- Decision: Hide pi cycle lifecycle events by default and show them only in verbose pretty output.
  Rationale: The default operator transcript should emphasize assistant output, tools, UI requests, and completion state. Pi lifecycle boundaries are useful for debugging but noisy during dogfood.
  Date: 2026-04-29

- Decision: Track observed Scherzo pass, displayed Scherzo pass, and pi cycle as separate renderer state.
  Rationale: Quiet mode hides pi lifecycle lines, but visible assistant/tool/UI output still needs a pass heading. Separating these concepts prevents a hidden `turn_start` from suppressing `Scherzo pass N`.
  Date: 2026-04-29

- Decision: Standardize the assistant block label as `assistant` without a colon.
  Rationale: The target transcripts already use a section-heading style for tools and the final validation examples use `assistant`; removing the colon keeps assistant, tool, input, and output blocks visually consistent.
  Date: 2026-04-29

- Decision: Update the main `README.md` control API section for attach examples instead of choosing between documentation files during implementation.
  Rationale: `README.md` contains the authoritative `Local control API and scherzoctl` documentation and currently shows the stale `turn`-oriented transcript; `.scherzo/README.md` is dogfood-workflow-specific and does not need an attach-renderer example for this change.
  Date: 2026-04-29

- Decision: Fix raw JSON UTF-8 truncation in this UI plan.
  Rationale: The attach UI cannot be considered improved if retained events can crash JSON encoding during replay. The fix is small and directly tied to attach reliability.
  Date: 2026-04-29

- Decision: Continue with the native Gleam renderer after the checked-in renderer spike.
  Rationale: The spike found no package-importable pi transcript renderer, found pi interactive components coupled to theme/TUI runtime behavior, and found that adding a Node helper would require new packaging and failure handling. The existing Gleam renderer is already pure, line-oriented, and test-covered.
  Date: 2026-04-29

## Outcomes & Retrospective

Spike checkpoint on 2026-04-29: `docs/spikes/pi-rendering-evaluation.md` recommends continuing with the native Gleam renderer. Pi's interactive components remain useful future reference material, but this plan should not add a Node helper or pi-renderer dependency for the current line-oriented attach transcript.

(To be filled after implementation. Include the final validation command output, a before/after attach transcript excerpt, and any recommendation for a future full-screen TUI or pi-renderer integration.)

## Context and Orientation

Scherzo is a Gleam Erlang-target project. Source code lives under `src/scherzo/`, tests live under `test/`, scripts live under `scripts/`, and execution plans live under `docs/plans/`. The validation command from the repository root is `direnv exec . gleam test`.

The local operator CLI is implemented in `src/scherzo/ctl.gleam`. It parses commands such as `ps`, `session`, `events`, `attach`, `abort`, `stop-after-turn`, `prompt`, and `ui respond`. The `attach` command replays retained session events through `ControlClient.get_events` and optionally follows live events through `ControlClient.stream_events`. Pretty mode calls functions in `src/scherzo/terminal/render.gleam`; raw and JSON modes use compact event lines and protocol JSON envelopes.

The pretty renderer is implemented in `src/scherzo/terminal/render.gleam`. It defines `RenderState`, `RenderOptions`, `RenderChunk`, `render_header`, `render_truncation_warning`, `render_event`, `render_events`, and `render_page`. It currently prints pi lifecycle event names `turn_start` and `turn_end` as `▶ turn N started` and `✓ turn N ended`, renders assistant deltas inline under an `assistant:` label, renders tool fields as single `input:`, `output:`, and `status:` lines, and hides most lifecycle events by default.

Terminal styling is in `src/scherzo/terminal/style.gleam`. It has a simple `ColorMode` with `ColorAuto`, `ColorAlways`, and `ColorNever`, and helper functions for headings, dim text, success, warning, error, assistant labels, and tool labels. `ColorAuto` currently behaves like no color in renderer tests.

Terminal sanitization is in `src/scherzo/terminal/sanitize.gleam`. Its current `text` function escapes all C0 controls, DEL, and C1 controls. Because newline is a C0 control, using `sanitize.text` on assistant or tool bodies turns normal line breaks into visible control pictures. That is safe but poor for readable assistant output and multiline tool output.

Session event types are in `src/scherzo/session/event.gleam`. `EventPayload` includes `kind`, `name`, `turn`, `pi_type`, `message`, `request_id`, `method`, `tool_name`, `tool_input`, `tool_output`, `tool_status`, `tokens`, and `raw_json`. `EventKind` includes `Lifecycle`, `Pi`, `AssistantMessage`, `Tool`, `UiRequest`, `UiResponse`, `TokenStats`, `Error`, and `PiRaw`.

Session JSON encoding is in `src/scherzo/session/json.gleam`. It encodes summaries, events, pages, payloads, token totals, and raw JSON. Raw pi JSON is represented as `event.RedactedRawJson(value, truncated)` and encoded under the nullable `raw_json` payload field.

Raw pi JSON redaction and truncation are split between `src/scherzo/session/redaction.gleam` and `src/scherzo_redaction_ffi.erl`. `redaction.max_raw_json_bytes` is `16_384`. The Erlang FFI currently truncates a binary by bytes using `binary:part(Value, 0, MaxBytes)`, which can cut a multibyte UTF-8 codepoint.

Existing tests relevant to this work are `test/terminal_render_test.gleam`, `test/ctl_attach_render_test.gleam`, `test/ctl_test.gleam`, `test/session_event_test.gleam`, `test/control_protocol_test.gleam`, and redaction/session tests under `test/`. The baseline on 2026-04-29 reports `278 passed, no failures`.

## Preconditions and Verified Facts

Before implementation, run these commands from the repository root:

    git status --short
    jj status --ignore-working-copy
    direnv exec . gleam test

The expected code baseline when this plan was written was `direnv exec . gleam test` ending with `278 passed, no failures`. During plan review on 2026-04-29 20:58Z, the working copy contained this plan as an uncommitted added document (`A docs/plans/operator-attach-ui-next.md`); that doc-only state is acceptable while reviewing the plan. Before implementation code edits begin, normalize the tree by committing the plan-only change or by recording the exact doc-only dirty state in Progress. Do not start production code edits with unrelated working-copy changes. If the test count, command output, or repository facts differ, update this section and the Progress section before editing code.

Current repository facts this plan depends on:

- `src/scherzo/ctl.gleam` defines `OutputMode` variants `Pretty`, `Raw`, and `Json`; `FollowMode` variants `Follow` and `NoFollow`; `Command.Events`; `Command.Attach`; `ControlClient`; and `Output` test seams.
- `src/scherzo/ctl.gleam` defaults `attach <session-id>` to pretty follow mode, accepts `attach --raw <session-id>`, accepts `attach --json <session-id>`, and preserves the legacy `attach --raw --json <session-id>` alias for JSON stream output.
- `src/scherzo/ctl.gleam` parses `events --pretty <session-id>` and pretty replay uses paginated retained events.
- `src/scherzo/terminal/render.gleam` currently has `RenderOptions(color_mode, show_lifecycle, show_raw_unknown)` and no CLI path to enable lifecycle/raw unknown output.
- `src/scherzo/terminal/sanitize.gleam` currently has only `text(String) -> String` and escapes newline as a control picture.
- `src/scherzo/agent/runner.gleam` assigns the Scherzo runner pass number to `PiUpdate.turn` and passes that through to session events.
- `src/scherzo/orchestrator/daemon.gleam` classifies pi `turn_start`, `turn_end`, `agent_start`, and `agent_end` as `session_event.Pi`, not as `Lifecycle`.
- `src/scherzo_redaction_ffi.erl` currently truncates redacted raw JSON by byte count without ensuring the returned prefix is valid UTF-8.

If any fact is false, do not guess. Re-read the named files, update this plan, and then continue.

## Scope Boundaries

In scope: a spike note comparing rendering reuse options; UTF-8-safe raw JSON truncation; clearer pretty attach terminology; default hiding of pi lifecycle cycle events; verbose pretty mode; block-oriented sanitization that preserves normal newlines; improved assistant body rendering; improved tool body rendering with display truncation; renderer and CLI tests; updated attach examples in the `README.md` `Local control API and scherzoctl` section.

Out of scope: Linear result comments; Linear command comments; changing EventHub retention; changing pi RPC event decoding beyond what the renderer already receives; durable transcript storage; full-screen TUI; keyboard navigation; scrollback search; interactive expand/collapse; answering UI requests from inside attach; changing raw or JSON event contracts; renaming existing non-attach operator surfaces such as `stop-after-turn`, the `current_turn` JSON field, or the `ps` table header; introducing a required Node or npm dependency unless this plan is explicitly revised after the spike.

## Milestones

Milestone 1 is the rendering spike. At the end, `docs/spikes/pi-rendering-evaluation.md` exists and records whether pi's package exports, pi's TUI primitives, a thin Node helper, or a third-party terminal library can realistically replace or augment the native renderer. This milestone comes first because choosing a renderer after refactoring the native renderer would waste work.

Milestone 2 stabilizes replay safety. At the end, redacted raw JSON truncation cannot create invalid UTF-8, and a regression test proves that a multibyte raw JSON payload larger than `redaction.max_raw_json_bytes` can still be encoded by `session_json.event_to_string` without crashing. This comes before UI changes because attach must be reliable before it can be readable.

Milestone 3 fixes terminology and default lifecycle noise. At the end, default pretty output groups events under `Scherzo pass N`, does not print pi `turn_start` or `turn_end` events, and never labels pi lifecycle events as plain `turn` lines. Verbose output can show pi cycle boundaries with the words `pi cycle`. This milestone also proves that a hidden `turn_start` does not suppress the first visible `Scherzo pass N` heading. This directly addresses the confusing dogfood output.

Milestone 4 improves text bodies. At the end, assistant output and tool output preserve ordinary line breaks, escape dangerous terminal controls, indent wrapped body lines consistently, and still support inline streaming without duplicating replayed cursors. The renderer has explicit state for whether an assistant line or a tool output subsection is open, so a newline in one delta and a continuation in the next delta do not lose indentation. This makes final assistant answers and tool outputs readable.

Milestone 5 improves tool and status rendering. At the end, tool events render as compact blocks with separate input/output/status subsections, long displayed output is visibly truncated by display policy, UI requests remain prominent, token summaries name the Scherzo pass, and selected Scherzo lifecycle events such as probe start/finish or worker exit can be shown without mixing them with assistant text.

Milestone 6 wires CLI verbosity and documentation. At the end, `scherzoctl attach --verbose <session-id>` and `scherzoctl events --pretty --verbose <session-id>` enable detailed pretty output, usage text documents the flag, tests cover parsing and rendering, and `README.md` shows the quiet and verbose attach examples in the main control API section.

## Plan of Work

Create a spike note at `docs/spikes/pi-rendering-evaluation.md`. The note should describe the current problem, the evaluation criteria, the commands run, and a recommendation. Evaluate five options: direct reuse of a pi transcript renderer if one exists; direct use of pi interactive components such as assistant and tool components; a thin Node helper that imports pi or pi TUI packages and renders JSON events; a third-party terminal rendering library; and the existing native Gleam renderer. Score each option in prose against portability, line-oriented replay/follow fit, testability, output quality, dependency cost, and failure behavior. Do not add a production dependency during the spike. If a temporary script is useful, put it under `scripts/spikes/` and either delete it before the spike commit or keep it only if it is small, documented, and does not require checked-in dependencies.

In the spike, first check what is available from the repository environment without making the shell session fail just because Node or pi is absent. From the repository root, run safe discovery commands such as:

    if command -v node >/dev/null 2>&1; then node --version; else echo "node: not found"; fi
    if command -v pi >/dev/null 2>&1; then command -v pi; pi --help 2>&1 | head -40; else echo "pi: not found"; fi

Record equivalent output in `docs/spikes/pi-rendering-evaluation.md`, but do not keep temporary files with absolute paths in the repository. If importing `@mariozechner/pi-coding-agent` from the repo root fails because the package is not a repo dependency, record that result. If an installed package can be located without hard-coding a machine-specific path, inspect its `package.json` exports and entry-point type declarations. Record whether a standalone replay renderer exists. Try, only if imports are available, to instantiate an assistant message component and render it to lines at width 80. Try the same for a tool component and record any required TUI runtime or tool-definition dependencies. The default recommendation should remain native Gleam unless a plain script can render representative Scherzo assistant and tool events without full-screen TUI coupling.

Fix raw JSON truncation in `src/scherzo_redaction_ffi.erl`. Replace byte-prefix truncation with UTF-8-safe truncation. The function should still cap by `MaxBytes`, still return `{Value, true}` when truncation occurs, and still fail closed to the existing placeholder if redaction itself fails. A concrete implementation is to take the byte prefix, pass it through `unicode:characters_to_binary(Prefix, utf8, utf8)`, and when Erlang returns `{incomplete, ValidPrefix, _}` or `{error, ValidPrefix, _}`, use `ValidPrefix` rather than the invalid original prefix. Keep ASCII placeholders unchanged.

Add the regression test to `test/session_redaction_test.gleam` unless a more focused session JSON test exists by implementation time. Construct raw JSON containing a string value with enough repeated multibyte characters to exceed `redaction.max_raw_json_bytes`. Call `redaction.redact_raw_json(raw, [])`, assert `truncated == True`, put the returned `event.RedactedRawJson` into an `event.EventPayload` as `raw_json: Some(redacted)`, and call `session_json.payload_to_string` or `session_json.event_to_string`. The test should pass by completing and by asserting the encoded JSON contains `"truncated":true`; before the fix it should reproduce or risk the `unexpected_end` crash. Do not rely on `string.length(redacted.value) <= redaction.max_raw_json_bytes` for the multibyte regression, because Gleam string length is not a byte-size oracle; the behavioral proof is that the truncated value remains valid for JSON encoding.

Update `src/scherzo/terminal/render.gleam` terminology. Rename internal helpers so future readers do not confuse concepts: `current_turn` should become `current_pass` or `current_scherzo_pass`; `turn_label` should become `pass_label`; `ensure_turn_heading` should become `ensure_pass_heading`; and visible default headings should say `Scherzo pass N`. Add separate state for the last observed pass and the last displayed pass heading, for example `current_pass: Option(Int)` and `displayed_pass: Option(Int)`. The public type can change if all tests are updated in the same commit. The old words may remain in payload field names because the event schema still calls the number `turn`.

Change default rendering for pi lifecycle events in `src/scherzo/terminal/render.gleam`. `payload.name == "turn_start"` and `payload.name == "turn_end"` should no longer call functions that print `turn N started` or `turn N ended` by default. A hidden `turn_start` may update `current_pass` from `payload.turn` and increment `pi_cycle`, but it must not update `displayed_pass`. `ensure_pass_heading` should emit `Scherzo pass N` whenever a visible event has `payload.turn: Some(N)` and `displayed_pass != Some(N)`, then set `displayed_pass: Some(N)`. Add `pi_cycle` state that increments on `turn_start`; cycle numbers are renderer-local to the displayed replay/follow stream rather than stable protocol identifiers. In verbose mode, render the pass heading if needed and then render `pi cycle <n> started` and `pi cycle <n> ended` using dim styling. If a `turn_end` appears before a `turn_start` in retained history, suppress it rather than rendering `pi cycle ? ended`, and keep that choice in the Decision Log.

Extend `RenderOptions` in `src/scherzo/terminal/render.gleam` to include verbose behavior explicitly. Keep `default_options(color_mode)` as the quiet operator view. Add `verbose_options(color_mode)` or a `verbosity` field so CLI code does not construct raw booleans at every call site. In verbose mode, set `show_lifecycle`, `show_raw_unknown`, and `show_pi_cycles` to true. In default mode, keep unknown raw payloads hidden and pi cycles hidden.

Add block sanitization in `src/scherzo/terminal/sanitize.gleam`. Keep the existing `text` function for inline labels and one-line fields. Add a new public function `block_lines(value: String) -> List(String)` that normalizes CRLF to LF, splits on LF, and applies control-character escaping to each resulting line. It should preserve LF line boundaries as layout while escaping ESC and other controls within each line; a lone carriage return should remain visible as an escaped control picture rather than moving the cursor. Add tests showing that `block_lines("a\nb")` returns `["a", "b"]`, and that an input containing `\u{1b}[31m` does not emit a real escape character.

Update assistant rendering in `src/scherzo/terminal/render.gleam`. Use `assistant` without a colon everywhere. When assistant deltas contain newlines, split and indent them as body lines rather than displaying `␊`. Preserve streaming behavior with explicit line state: when an assistant block opens, emit `assistant` as a line, then prefix the first body fragment with two spaces; when a sanitized delta contains LF, finish the current output line, emit any complete following lines with the same two-space prefix, and leave the final partial line open for the next delta. Consecutive `message_update` deltas such as `"Hello "` and `"world"` should still render as `Hello world` on one body line. Deltas such as `"first\n"` followed by `"second"` should render as two indented body lines. If preserving streaming and multiline blocks conflict, prefer readable replay and record any live-output compromise in Surprises & Discoveries.

Update tool rendering in `src/scherzo/terminal/render.gleam`. Keep the tool label based on `payload.tool_name` when present. Replace single-line `input:` and `output:` rendering with subsections. A tool with input and output should render like:

    tool bash
      input
        gleam test
      output
        278 passed, no failures
      status: success

For a one-line input or output, this block form is still acceptable because it keeps multiline behavior predictable. Display no empty subsection. Track the active tool label and active subsection, for example `active_tool_label: Option(String)` and `active_tool_section: Option(ToolSection)`. If repeated update events for the same tool only add output, do not repeat the tool label or the `output` subsection heading; append additional sanitized output body lines under the open output subsection. A new tool input, a different tool label, a status/end event, or any non-tool event closes the active tool subsection.

Add display truncation helpers in `src/scherzo/terminal/render.gleam` or a small private helper module under `src/scherzo/terminal/`. Use constants such as `default_max_body_lines = 40` and `default_max_body_line_chars = 200` unless the spike recommends a better width-aware primitive. Truncation should be visible, for example `… [display truncated; use --json for retained raw event]`. This display truncation is separate from storage truncation in `runner.gleam`; it protects the operator terminal.

Keep UI request rendering prominent. `UiRequest` should still render as a warning-colored line with method and request id, followed by a body block if `payload.message` is present. The line should make the action clear, such as `UI request waiting: confirm #ui-1`. `UiResponse` can remain dim in default output because it indicates an operator response was recorded.

Adjust token and lifecycle status rendering. Token summaries should refer to the Scherzo pass when the pass number is known, such as `Scherzo pass 1 tokens: input=... total=...`; if no pass is known, render the existing neutral `tokens: input=... total=...` wording. Selected Scherzo lifecycle events with no raw JSON, such as `probe_started`, `probe_finished`, `pi_session_started`, `worker_started`, and `worker_exited`, may be shown in verbose mode only. Do not show pi `agent_start` and `agent_end` in default mode.

Wire `--verbose` through `src/scherzo/ctl.gleam`. Add a `verbose: Bool` field to the internal `Flags` record. Extend `Command.Events` and `Command.Attach` with a `verbose` boolean or a small pretty-options value. Keep the change simple and update every pattern match immediately. Add `--verbose` to `parse_flags`, usage text, and tests in `test/ctl_test.gleam`. In pretty mode, choose `render.verbose_options(color)` when verbose is true; otherwise choose `render.default_options(color)`. In raw and JSON modes, `--verbose` should have no effect rather than changing output contracts.

Update `test/terminal_render_test.gleam`. Add tests for default hiding of repeated pi cycle events, verbose pi cycle labeling, Scherzo pass headings, assistant multiline body rendering, terminal-control escaping in block bodies, tool multiline block rendering, display truncation, UI request body rendering, and token summary wording. Replace exact expectations that currently contain `▶ turn 1 started` or `✓ turn 1 ended` with the new transcript shape.

Update `test/ctl_attach_render_test.gleam`. Add a replay-plus-follow test where replay contains `turn_start` and live contains duplicate cursors, another `turn_start`, assistant output, and `turn_end`. The default transcript must contain the assistant text and must not contain `turn 1 started`, duplicate live text, or pi lifecycle noise. Add a verbose test that proves pi cycle lines appear only when requested.

Update documentation in `README.md`, specifically the `Local control API and scherzoctl` section that currently describes `attach` as grouping by turn. Replace the stale example with a quiet `Scherzo pass` transcript, mention `--verbose`, `--raw`, and `--json`, and keep `.scherzo/README.md` unchanged unless implementation discovers that its dogfood supervision snippet became misleading. Do not document any Node or pi-renderer dependency unless the spike changes the implementation direction.

## Concrete Steps

1. From the repository root, confirm the baseline:

       git status --short
       jj status --ignore-working-copy
       direnv exec . gleam test

   Expect no unrelated production-code changes and `278 passed, no failures` if the tree has not changed since authoring. If this plan is still uncommitted, `git status --short` may show only `A docs/plans/operator-attach-ui-next.md` or `M docs/plans/operator-attach-ui-next.md`; commit the plan or record that doc-only state in Progress before beginning code edits.

2. Create `docs/spikes/` if it does not exist, then create `docs/spikes/pi-rendering-evaluation.md` with headings for purpose, candidates, commands run, findings, recommendation, and follow-up.

3. Run the safe spike discovery commands from the repository root as described in Plan of Work. Record whether Node is available, whether `command -v pi` resolves, whether importing `@mariozechner/pi-coding-agent` from the repo root succeeds, and whether any available pi package export provides a standalone transcript renderer. The commands should continue to the recommendation even when Node or pi is missing.

4. If a temporary spike script is needed, create it under `scripts/spikes/`, run it, copy the important output into `docs/spikes/pi-rendering-evaluation.md`, and then either delete the script or keep it with a short comment explaining how to run it. Do not add production dependencies in this step.

5. Update this plan's Decision Log with the spike recommendation, then stop at a spike summary checkpoint before production renderer edits. The summary must name the evaluated renderer options, the evidence that mattered, the recommendation, and whether the remaining plan still follows the native Gleam path. If an operator or reviewer is available, report that summary to them before continuing. If the implementation is running unattended, record the same summary in this plan's Progress or Outcomes section before continuing. If the recommendation is not the native Gleam path, stop and revise the remaining milestones before implementing them.

6. Edit `src/scherzo_redaction_ffi.erl` to make truncation UTF-8 safe as described in Plan of Work.

7. Add the multibyte truncation regression test to `test/session_redaction_test.gleam` unless implementation-time inspection identifies a more focused session JSON test file. Run the targeted test command if available, otherwise run `direnv exec . gleam test`. Before the fix, this test should fail or crash; after the fix, it should pass by JSON-encoding the truncated redacted value.

8. Commit the spike and UTF-8 safety work after `direnv exec . gleam test` passes. Suggested commit message: `fix(scherzoctl): stabilize attach replay rendering substrate`.

9. Edit `src/scherzo/terminal/render.gleam` to rename renderer state and helper terminology from turn to Scherzo pass. Add separate observed-pass and displayed-pass state in the same edit. Update affected tests in `test/terminal_render_test.gleam` in the same step.

10. Change pi `turn_start` and `turn_end` rendering to be hidden by default and labeled as `pi cycle` in verbose mode. Add tests for default and verbose behavior, including a quiet stream where a hidden `turn_start` is followed by assistant output and still emits `Scherzo pass 1`.

11. Add `sanitize.block_lines` in `src/scherzo/terminal/sanitize.gleam` and tests for newline preservation plus escape safety.

12. Update assistant rendering to use block-aware sanitization for multiline content. Add exact transcript tests for a two-line assistant message, a newline split across two deltas, ordinary adjacent deltas that remain on one line, and terminal escape input.

13. Update tool rendering to use block subsections for input, output, and status. Add exact transcript tests for single-line output, multiline tool output, repeated output-only updates that do not repeat the tool or output heading, and status/end events that close the active tool section.

14. Add display truncation helpers and tests proving long tool output includes a visible truncation note.

15. Run `direnv exec . gleam test`. Commit the renderer terminology and body-rendering work after it passes. Suggested commit message: `feat(scherzoctl): clarify pretty attach transcript rendering`.

16. Edit `src/scherzo/ctl.gleam` to parse `--verbose`, carry it through `Command.Events` and `Command.Attach`, and select default or verbose render options in pretty mode.

17. Update `test/ctl_test.gleam` for parsing and usage text. Update `test/ctl_attach_render_test.gleam` for default and verbose attach behavior.

18. Update the `README.md` `Local control API and scherzoctl` section with the improved attach examples and the `--verbose` flag.

19. Run the full validation:

       direnv exec . gleam format --check src test
       direnv exec . gleam test
       direnv exec . gleam run -- ctl --help

   Expect formatting to pass, all tests to pass, and help output to mention `--verbose` for pretty `attach` and `events --pretty` usage.

20. If a local control file is available, run a manual smoke test:

       export SCHERZO_CONTROL_FILE=.scherzo/workspaces/research/.scherzo-state/control.json
       scripts/scherzoctl attach --no-follow <session-id>
       scripts/scherzoctl attach --no-follow --verbose <session-id>

   Expect default output to show Scherzo pass headings, assistant and tool blocks, and no repeated `turn 1 started` lines even when retained events begin with pi `turn_start`. Expect verbose output to include pi cycle lines.

21. Update this plan's Progress, Surprises & Discoveries, and Outcomes & Retrospective with the final test count and a short before/after transcript excerpt. Commit the CLI/docs work. Suggested commit message: `docs(scherzoctl): document improved attach verbosity` or combine with the CLI commit if the diff is small.

## Testing and Falsifiability

The spike is falsifiable by its written recommendation and by the spike summary checkpoint. It must prove a pi or third-party renderer is usable with a small command or script from the repository root. If it cannot render both an assistant message and a representative tool event without hard-coded machine-local paths, full-screen TUI coupling, or new production dependencies, the spike has disproved renderer replacement for this phase and the native path proceeds. The plan is incomplete if the implementation reaches production renderer edits without a checked-in spike note and a human-readable summary of what the spike found.

The UTF-8 fix is falsifiable with a multibyte truncation test. The test should build raw JSON with repeated multibyte characters, call `redaction.redact_raw_json`, embed the returned `RedactedRawJson` in a session payload, and call `session_json.payload_to_string` or `session_json.event_to_string`. Failure is any crash, any invalid JSON encoding result, or a missing `"truncated":true` marker. The test should not depend on Gleam `string.length` as a byte-count check for the multibyte case.

Renderer terminology is falsifiable with exact transcript tests. A default event sequence containing two pi `turn_start`/`turn_end` pairs with `turn: Some(1)` and one assistant message must render a transcript that contains `Scherzo pass 1`, contains the assistant message, and does not contain `turn 1 started`, `turn 1 ended`, or duplicate pass headings. A separate default test must start with a hidden `turn_start` followed by assistant output and assert that the pass heading still appears, proving that hidden lifecycle state did not mark the heading as displayed. A verbose render of the same events must contain `pi cycle 1 started` and `pi cycle 2 started`; the cycle numbers are local to the rendered stream.

Block sanitization is falsifiable with exact function tests. `sanitize.block_lines("hello\nworld")` must produce two lines. An input containing ESC, OSC, CSI, DEL, carriage return, or C1 controls must not produce the raw control bytes in any output line. The existing `sanitize.text` behavior should remain unchanged for one-line labels.

Assistant rendering is falsifiable with transcript tests that feed one `message_update` containing `"first\nsecond"` and a two-delta sequence `"first\n"` followed by `"second"`. The output must show two indented body lines rather than `first␊second` and must not drop indentation on the second delta. A separate test should feed two deltas, `"Hello "` and `"world"`, and assert they still render as continuous assistant text on one body line under the `assistant` label.

Tool rendering is falsifiable with transcript tests for input, multiline output, status, repeated updates, and display truncation. The output must show separate `input`, `output`, and `status` sections, preserve output line breaks, and include a visible truncation note when the display line limit is exceeded. Repeated output-only updates for the same tool must append body lines under the existing output section rather than repeating `tool bash` or `output`; a status/end event must close the active tool section.

CLI behavior is falsifiable with parser and injected-client tests. `ctl.parse(["attach", "--verbose", "ABC-1"])` and `ctl.parse(["events", "--pretty", "--verbose", "ABC-1"])` must preserve pretty mode and set the verbose option. `attach --json --verbose` and default compact `events --verbose` must still emit their existing JSON or compact raw contracts, not pretty text. Replay-to-follow duplicate suppression must continue to skip duplicate cursors in default and verbose pretty modes.

The full suite must pass with:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

This plan is disproved if the improved default pretty transcript still contains repeated `turn 1 started` lines for pi lifecycle events, if raw or JSON attach output changes shape, if invalid UTF-8 raw JSON can still crash replay encoding, or if the implementation requires a new production Node dependency without a revised Decision Log and plan update.

## Validation and Acceptance

Acceptance is operator-visible. Given a retained session with pi lifecycle events, assistant output, a tool call, a token summary, and a worker exit, running:

    scripts/scherzoctl attach --no-follow <session-id>

should produce a quiet transcript shaped like:

    LIV-11 Dogfood Scherzo research workflow for Linear result UX
    workspace: .scherzo/workspaces/research/LIV-11
    session: LIV-11--576460751544-1
    status: exited

    Scherzo pass 1
    assistant
      I will inspect the repository and propose a Linear-ready result summary.
    tool bash
      input
        direnv exec . gleam test
      output
        278 passed, no failures
      status: success
    Scherzo pass 1 tokens: input=... output=... total=...

The exact issue title and token values will vary, but the default output must not show repeated `turn 1 started` or `turn 1 ended` lines.

Running:

    scripts/scherzoctl attach --no-follow --verbose <session-id>

should include additional diagnostic lines such as pi cycle boundaries and unknown raw event names. Those diagnostic lines must use `pi cycle` for pi lifecycle boundaries.

Running:

    scripts/scherzoctl attach --raw --no-follow <session-id>
    scripts/scherzoctl attach --json --no-follow <session-id>
    scripts/scherzoctl events --json <session-id>

should preserve the existing compact and JSON contracts. Pretty formatting, ANSI labels, multiline body rendering, and verbose lifecycle lines must not appear in raw or JSON output.

The final implementation is accepted only after the full validation commands pass, the plan's Progress and Outcomes sections are updated, and the spike note clearly records why the chosen renderer path was selected.

## Rollout, Recovery, and Idempotence

This change is local to CLI rendering and raw JSON truncation. It does not change worker scheduling, Linear state transitions, workflow configuration, EventHub retention, or the control protocol's raw event data. If the pretty renderer regresses, operators can immediately fall back to `scripts/scherzoctl attach --raw <session-id>` or `scripts/scherzoctl attach --json <session-id>` while the pretty commit is reverted.

The UTF-8 truncation fix is safe to roll out because it only changes how already-redacted raw JSON is shortened. It may return slightly fewer bytes than before when truncation would otherwise split a multibyte codepoint. That is preferable to returning invalid UTF-8. If this change causes unexpected behavior, revert only the FFI truncation commit and keep the spike note; pretty rendering changes are independent.

The spike is idempotent. Re-running its commands should update only `docs/spikes/pi-rendering-evaluation.md` and any explicitly kept script under `scripts/spikes/`. Do not check in machine-local absolute paths or secrets from pi logs.

The CLI `--verbose` flag is additive. Existing invocations keep their old mode selection except for improved pretty presentation. Raw and JSON output modes remain the recovery path for automation.

## Artifacts and Notes

The observed bad default transcript during dogfood had repeated lifecycle lines similar to:

    ▶ turn 1 started
    ✓ turn 1 ended
    ▶ turn 1 started
    assistant:
      ...
    ✓ turn 1 ended

The target quiet transcript should instead look like:

    Scherzo pass 1
    assistant
      ...

The target verbose transcript may include:

    Scherzo pass 1
    pi cycle 1 started
    pi cycle 1 ended
    pi cycle 2 started
    assistant
      ...
    pi cycle 2 ended

The renderer spike should produce `docs/spikes/pi-rendering-evaluation.md`. A useful outline for that file is:

    # Pi and third-party rendering evaluation

    ## Question

    Can Scherzo reuse an existing renderer for `scherzoctl attach`?

    ## Candidates

    Native Gleam renderer, pi transcript renderer, pi interactive components, pi-tui primitives, thin Node helper, third-party terminal library.

    ## Commands and Evidence

    Record commands and short outputs here. Do not include secrets or machine-local paths unless they are clearly marked as local observations.

    ## Recommendation

    State the selected path and why.

## Interfaces and Dependencies

The preferred implementation keeps production code in Gleam and Erlang. No new production package dependency is required unless this plan is revised after the spike.

In `src/scherzo/terminal/sanitize.gleam`, keep:

    pub fn text(value: String) -> String

Add:

    pub fn block_lines(value: String) -> List(String)

`block_lines` returns sanitized display lines and preserves ordinary LF line boundaries as layout.

In `src/scherzo/terminal/render.gleam`, keep the public rendering entry points:

    pub fn initial_state(since_cursor: Int) -> RenderState
    pub fn default_options(color_mode: style.ColorMode) -> RenderOptions
    pub fn chunks_to_string(chunks: List(RenderChunk)) -> String
    pub fn render_header(summary: event.SessionSummary, options: RenderOptions) -> List(RenderChunk)
    pub fn render_truncation_warning(options: RenderOptions) -> List(RenderChunk)
    pub fn render_event(state: RenderState, stored_event: event.SessionEvent, options: RenderOptions) -> #(RenderState, List(RenderChunk))
    pub fn render_events(state: RenderState, events: List(event.SessionEvent), options: RenderOptions) -> #(RenderState, List(RenderChunk))
    pub fn render_page(summary: event.SessionSummary, page: event.EventPage, options: RenderOptions) -> List(RenderChunk)

`RenderState` should carry, with these names or clear equivalents, `last_cursor`, `current_pass`, `displayed_pass`, `pi_cycle`, assistant line-open state, `active_tool_label`, and active tool subsection state. Do not use a single field to mean both the observed pass and the displayed pass heading.

Add one of these two interfaces and use it consistently:

    pub fn verbose_options(color_mode: style.ColorMode) -> RenderOptions

or:

    pub type RenderVerbosity {
      Quiet
      Verbose
    }

    pub fn options(color_mode: style.ColorMode, verbosity: RenderVerbosity) -> RenderOptions

Choose the smallest change that keeps call sites clear. `default_options` must represent quiet operator output with lifecycle, raw unknown, and pi-cycle diagnostics disabled. Verbose options must enable lifecycle, raw unknown, and pi-cycle diagnostics. Do not expose a broad renderer plugin interface in this phase.

In `src/scherzo/ctl.gleam`, add a `verbose` flag to `Flags` and carry it to pretty `Events` and `Attach`. A simple acceptable shape is:

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

If this constructor churn is too noisy, define a small `PrettyFlags` record instead, but do not add a general configuration object unless it is used in more than one place.

In `src/scherzo_redaction_ffi.erl`, preserve the exported functions:

    redact_raw_json/3
    redact_raw_json_fail_closed/4

Change only the implementation of truncation so returned binaries are valid UTF-8. The Gleam-facing API in `src/scherzo/session/redaction.gleam` should not change.
