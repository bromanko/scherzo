# Pi and third-party rendering evaluation

## Question

Can Scherzo reuse an existing renderer for `scherzoctl attach` instead of refining the native Gleam renderer in `src/scherzo/terminal/render.gleam`?

The required renderer must accept Scherzo's retained `session.EventPayload` stream, replay old events, follow new events, render to ordinary line-oriented terminals and logs, remain snapshot-testable from Gleam tests, and fail closed to a usable transcript. It must not require a project-local Node dependency or a full-screen TUI runtime for this phase.

## Candidates

The candidates evaluated were: a direct pi transcript renderer, pi interactive components, pi TUI primitives through a thin helper, a third-party terminal rendering library, and Scherzo's existing native Gleam renderer.

## Commands and Evidence

The repository baseline before the spike was checked with:

    git status --short
    jj status --ignore-working-copy
    direnv exec . gleam test

The working copy contained only the new plan document, and `direnv exec . gleam test` ended with:

    278 passed, no failures

Node and pi discovery were run from the repository root with commands equivalent to:

    if command -v node >/dev/null 2>&1; then node --version; else echo "node: not found"; fi
    if command -v pi >/dev/null 2>&1; then command -v pi; pi --help 2>&1 | head -40; else echo "pi: not found"; fi
    node -e "try { require.resolve('@mariozechner/pi-coding-agent/package.json') } catch (e) { console.log(e.message) }"

The local environment reported Node `v22.22.2`, found a `pi` executable in the user's profile, and printed normal pi CLI help. Importing `@mariozechner/pi-coding-agent` from the Scherzo repository root failed with `Cannot find module '@mariozechner/pi-coding-agent/package.json'`, which means Scherzo cannot import pi by package name without adding a production Node dependency or relying on a machine-local global install.

The installed pi package behind the CLI was inspected without adding it as a Scherzo dependency. Its package metadata reports `@mariozechner/pi-coding-agent` version `0.70.2`, package type `module`, main entry `./dist/index.js`, declarations `./dist/index.d.ts`, and exports only the package root and `./hooks`. The root declarations export interactive pieces such as `AssistantMessageComponent`, `ToolExecutionComponent`, `UserMessageComponent`, `renderDiff`, `Theme`, `RpcClient`, `runRpcMode`, and `runPrintMode`. The declarations do not expose a standalone transcript renderer that accepts arbitrary stored Scherzo/pi JSON events and emits a plain replay transcript.

A small dynamic-import probe loaded the installed package entry by file URL, without checking in a script. The package exports did include `AssistantMessageComponent` and `ToolExecutionComponent`, but did not include a value named `MessageRenderer`. Constructing an assistant component without theme initialization failed with `Theme not initialized. Call initTheme() first.` After calling `initTheme('default', false)` and `getMarkdownTheme()`, `AssistantMessageComponent.render(80)` returned padded terminal lines and pi-owned control sequences such as OSC prompt markers. A representative first result included lines shaped like:

    "\u001b]133;A\u0007"
    " Hello world ..."
    " \u001b[38;2;128;128;128m```sh\u001b[39m ..."
    "\u001b]133;B\u0007\u001b]133;C\u0007 ..."

The same probe could construct a `ToolExecutionComponent`, but calling `markExecutionStarted()` failed with `Cannot read properties of undefined (reading 'requestRender')` when no TUI object was provided. The declaration for `ToolExecutionComponent` requires a `TUI` instance, a current working directory, optional tool definitions, and mutable rendering lifecycle calls, which does not fit Scherzo's current pure renderer tests.

## Findings

Direct pi transcript renderer: no stable package export was found for replaying arbitrary stored events into a line transcript. The exported `runPrintMode` and `runRpcMode` are pi runtime modes, not small rendering functions for Scherzo's stored event model. This option scores well only if such an API exists, and the spike did not find one.

Pi interactive components: assistant rendering can be made to return lines after theme initialization, but those lines are designed for pi's interactive terminal environment. They are padded to a width and include pi-owned OSC/ANSI sequences. Tool rendering depends on a TUI object and renderer lifecycle. This option has good output quality inside pi, but poor portability, poor failure isolation, and poor fit for simple logs.

Thin Node helper around pi or pi TUI primitives: a helper could import the installed package by absolute file URL in this local environment, but importing by package name from the repository fails because Scherzo does not depend on the package. Making this portable would require adding a Node dependency, packaging a helper process, defining supervision and fallback behavior, and snapshot-testing terminal control output. The quality gain for this phase does not justify that cost.

Third-party terminal rendering library: libraries such as Ink, terminal-kit, chalk, marked, cli-highlight, or a Markdown highlighter could improve formatting, but Scherzo currently has no Node package setup. Adding one only for a line transcript introduces a second runtime and dependency tree before the UI requirements are stable. A third-party renderer remains plausible later if Scherzo builds a full-screen TUI or export format.

Native Gleam renderer: the existing renderer is pure, line-oriented, already wired through `scherzoctl attach` and `events --pretty`, and already covered by exact transcript tests. It lacks the desired terminology, block rendering, and truncation behavior, but those gaps are smaller than adding a portable helper stack. It also fails closed: if pretty output is insufficient, operators can use `--raw` or `--json` without involving another process.

## Recommendation

Proceed with the native Gleam renderer for this plan. The spike did not find a stable, portable, low-risk drop-in pi transcript renderer. Pi's exported interactive components are useful evidence for future UX direction, but they are coupled to theme/TUI runtime behavior and emit control sequences that are not appropriate for Scherzo's current plain attach transcript.

The remaining production work should follow the native path described in `docs/plans/operator-attach-ui-next.md`: fix UTF-8-safe raw JSON truncation, rename displayed grouping to `Scherzo pass`, hide pi cycles by default, add verbose pretty output, preserve safe newlines, and improve tool blocks. A future full-screen TUI or HTML/Markdown transcript exporter can revisit pi components when Scherzo is ready to accept a Node/runtime dependency and an interactive rendering model.

## Follow-up

No production dependency was added during the spike. No temporary spike script was kept. If a future plan evaluates pi rendering again, it should begin by checking whether pi exposes a documented, package-importable transcript renderer and whether `ToolExecutionComponent` has a supported non-TUI rendering context.
