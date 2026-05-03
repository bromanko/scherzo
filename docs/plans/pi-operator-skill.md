# Add a pi operator skill for Scherzo

This ExecPlan is a living document. The sections Progress, Surprises & Discoveries, Decision Log, and Outcomes & Retrospective must be kept up to date as work proceeds.

## Purpose / Big Picture

After this change, an operator can open a normal pi session in the Scherzo repository and load a project skill named `scherzo-operator`. The operator can ask pi to summarize current Scherzo work, inspect recent events for a session, and prepare safe operator actions without memorizing the `scherzoctl` command surface. The visible proof is that `/skill:scherzo-operator`, or the fallback `pi --skill .pi/skills/scherzo-operator`, loads instructions that make pi run read-only `scripts/scherzoctl ... --json` commands first, summarize active sessions from structured JSON, and ask for explicit confirmation before any state-changing command.

This phase does not add a custom pi extension or new typed tools. Scherzo already exposes a local authenticated control API through `scripts/scherzoctl`; a project skill is the smallest useful layer on top of that interface. It gives operators an agent-guided workflow while keeping all real control behavior inside the existing daemon and CLI.

## Problem Framing and Constraints

Scherzo now has a usable local operator CLI, but using it well still requires a human or agent to remember several safety details: command options come after the `scherzoctl` subcommand, JSON output is the reliable format for automation, `attach` follows by default and can block, mutating commands have different confirmation flags, and the control file contains a token that must not be exposed. A pi skill can encode those habits directly in the repository so any pi session started here can operate Scherzo consistently.

The skill must be safe because it can instruct an agent to run powerful commands. It must bias toward read-only inspection, require user confirmation before every state-changing command, use JSON output for machine parsing, avoid exposing control tokens or API keys in summaries, and remind the agent that Scherzo's control API is local to the daemon host. The skill must not scan broad filesystem locations for control files. It should use the explicit `--control-file <path>` option, the `SCHERZO_CONTROL_FILE` environment variable, or the repository default `.scherzo/workspaces/.scherzo-state/control.json` when that file exists.

The current repository already has `.pi/skills/` with project review and planning skills. The new skill must live alongside them under `.pi/skills/scherzo-operator/` and must not change existing skills. Pi skill commands can be disabled by user settings, so the README must document both `/skill:scherzo-operator` and the explicit startup form `pi --skill .pi/skills/scherzo-operator`.

## Strategy Overview

Add a project-level pi skill under `.pi/skills/scherzo-operator/SKILL.md`. Pi 0.70.2 skill documentation says project skills are discovered from `.pi/skills/`, directories containing `SKILL.md` are discovered recursively, `name` is required, `description` is required, the name must be lowercase letters, numbers, and hyphens with no leading, trailing, or consecutive hyphens, and the name must match the parent directory. The skill's frontmatter name will be `scherzo-operator`, and its description will explain when to use the operator workflow.

Keep `SKILL.md` concise. It should define the operating policy: discover the control file safely, run read-only JSON commands first, prefer `events --json` for summaries instead of blocking `attach`, ask for confirmation before mutations, redact secrets, and report command statuses clearly. Put detailed command examples and response-shape notes in `.pi/skills/scherzo-operator/references/commands.md`, linked from the main skill with a relative path from the skill directory.

Add deterministic tests in `test/skill_docs_test.gleam`. These tests cannot prove model behavior, but they can prove that the skill package is discoverable, references the current `scherzoctl` command surface, includes the safety rules, explains response statuses, and that README documents how an operator loads the skill. The implementation remains additive: no Scherzo daemon, control protocol, scheduler, or pi RPC code changes are required.

## Alternatives Considered

One alternative is to build a custom pi extension that exposes typed Scherzo tools directly. That would be a richer agent UI, but it adds extension packaging, a second integration surface, and tool-level lifecycle concerns before the CLI-based workflow has been used. The current `scripts/scherzoctl ... --json` interface is already enough for a safe first operator skill.

Another alternative is to tell operators to prompt pi manually with ad hoc instructions. That is error-prone because the agent may parse pretty terminal output, run `attach` and block when a bounded `events --json` call would do, skip confirmation before mutating commands, or expose the control token while explaining the control file.

A third alternative is to document the operator workflow only in `README.md`. That helps humans but does not make pi load the workflow on demand. A project skill is discoverable by pi and can be invoked explicitly with `/skill:scherzo-operator` when skill commands are enabled.

A fourth alternative is to place the skill under `.agents/skills/` for broader harness compatibility. This repository already uses `.pi/skills/`, and the requested behavior is pi-specific. Cross-harness packaging can be revisited later if another agent needs the same workflow.

## Risks and Countermeasures

The main safety risk is the operator agent taking destructive or state-changing actions without explicit user intent. Countermeasure: the skill must require confirmation before `pause`, `resume`, `reload`, `retry`, `park`, `unpark`, `abort`, `stop-after-turn`, `prompt`, `ui respond`, or any command that uses `--yes`. The validation tests check for confirmation language and the mutating command names.

The main privacy risk is the agent exposing control tokens, Linear API keys, repository URLs, or sensitive issue content in summaries. Countermeasure: the skill must say never to reveal token values, never to print `LINEAR_API_KEY`, to summarize rather than quote large raw payloads by default, and to redact sensitive content unless the user explicitly asks for a raw excerpt.

The main reliability risk is the agent parsing pretty output or opening an unbounded stream. Countermeasure: the skill must prefer `ps --json`, `session --json`, and `events --json` for inspection. It may mention `attach` for human live watching, but it must warn that `attach` follows by default and should not be used for ordinary summaries unless the user wants a live stream or `--no-follow` is specified.

The main command-accuracy risk is documenting invalid `scherzoctl` invocation order. `src/scherzo/main.gleam` routes the first argument after `ctl` as the command name, and `src/scherzo/ctl.gleam` parses options after that command. Countermeasure: all examples must use command-first forms such as `scripts/scherzoctl ps --json --control-file <path>`, not `scripts/scherzoctl --control-file <path> ps --json`. A test should reject the most tempting invalid example shape.

The main environment risk is the pi session not knowing where the daemon control file is. Countermeasure: the skill instructs the agent to honor `SCHERZO_CONTROL_FILE`, use `--control-file <path>` when the user provides a path, try only Scherzo's documented repository default when appropriate, and otherwise ask the user for the `control_server_started` log line or control file path.

The main discovery risk is assuming `/skill:scherzo-operator` always exists. Pi can disable slash skill commands through settings. Countermeasure: README documents both `/skill:scherzo-operator` and `pi --skill .pi/skills/scherzo-operator`, and the skill itself requires no settings change.

## Progress

- [x] (2026-04-28 18:40Z) Read pi's skills documentation and confirmed project skills can live under `.pi/skills/<name>/SKILL.md` with required `name` and `description` frontmatter.
- [x] (2026-05-02 17:20Z) Re-read pi 0.70.2 `docs/skills.md` and the SDK/example skill material. Confirmed recursive discovery of directories containing `SKILL.md`, name and description limits, and `/skill:<name>` behavior when skill commands are enabled.
- [x] (2026-05-02 17:20Z) Inspected the current `.pi/skills/` tree. It now exists and contains project skills such as `exec-plan`, `exec-plan-review`, and Gleam review skills; it does not contain `scherzo-operator`.
- [x] (2026-05-02 17:21Z) Inspected `src/scherzo/ctl.gleam`, `src/scherzo/control/command.gleam`, `src/scherzo/control/file.gleam`, `scripts/scherzoctl`, and `README.md` to normalize this plan against the current CLI and control-file behavior.
- [x] (2026-05-02 17:21Z) Ran `direnv exec . gleam test`; the current baseline reports `533 passed, no failures` after expected crash-report output from tests that intentionally exercise crash handling.
- [x] (2026-05-02 17:24Z) Revised this plan to remove stale prerequisite language, record the existing `.pi/skills/` directory, include the complete current `scherzoctl` mutating command surface, and document the command-first `--control-file` option ordering.
- [x] (2026-05-02 17:25Z) Re-ran `direnv exec . gleam test` after this documentation-only revision; it still reports `533 passed, no failures` with expected crash-report noise.
- [x] (2026-05-02 20:02Z) Verified the implementation workspace was clean with `jj status --color=never` and re-ran the baseline `direnv exec . gleam test`; it reported `533 passed, no failures` with the expected crash-report noise.
- [x] (2026-05-02 20:03Z) Added `test/skill_docs_test.gleam` with five deterministic tests for skill frontmatter, safety rules, command references, response statuses, and README operator-flow documentation.
- [x] (2026-05-02 20:03Z) Ran the red phase with `direnv exec . gleam test`; the new tests failed as intended because the skill files and README section were still absent, ending with `533 passed, 5 failures`.
- [x] (2026-05-02 20:04Z) Added `.pi/skills/scherzo-operator/SKILL.md` and `.pi/skills/scherzo-operator/references/commands.md`; a follow-up `direnv exec . gleam test` reported `537 passed, 1 failures`, proving the skill/reference tests passed and only README documentation remained.
- [x] (2026-05-02 20:04Z) Updated `README.md` with a `Using pi as an operator UI` section documenting `/skill:scherzo-operator`, the explicit `pi --skill .pi/skills/scherzo-operator` fallback, `SCHERZO_CONTROL_FILE`, read-only JSON summaries, and confirmation before mutations.
- [x] (2026-05-02 20:08Z) Ran final `direnv exec . gleam format --check src test`; it passed with no formatting changes required.
- [x] (2026-05-02 20:08Z) Ran final `direnv exec . gleam test` after the last README and test wording adjustments; it reported `538 passed, no failures` with the expected crash-report noise.
- [x] (2026-05-02 20:05Z) Skipped the optional manual pi skill-load dry-run because this implementation workflow is non-interactive and should not launch a live model session; deterministic tests now validate the load paths and safety guidance.
- [x] (2026-05-02 20:12Z) Reviewed the Gleam review feedback; no remaining findings required additional code or documentation edits beyond the already-applied review hardening. Ran post-review `direnv exec . gleam format --check src test` and `direnv exec . gleam test`; the test run reported `538 passed, no failures` with expected crash-report noise.

## Surprises & Discoveries

- Observation: The original plan's statement that the repository had no `.pi/skills/` directory became stale before implementation.
  Evidence: The tree already had `.pi/skills/exec-plan/SKILL.md`, `.pi/skills/exec-plan-review/SKILL.md`, and several Gleam review skills when implementation began. This phase added `.pi/skills/scherzo-operator/` as a sibling directory.

- Observation: The read-only control API and mutating operator controls are now implemented in the current tree.
  Evidence: `src/scherzo/ctl.gleam` parses `ping`, `ps`, `session`, `events`, `attach`, `pause`, `resume`, `reload`, `retry`, `park`, `unpark`, `abort`, `stop-after-turn`, `prompt`, and `ui respond`; `src/scherzo/control/command.gleam` defines the shared command result statuses.

- Observation: `--control-file` is a command option, not a top-level `scripts/scherzoctl` option.
  Evidence: `src/scherzo/main.gleam` treats the first argument after `ctl` as the control subcommand, and `src/scherzo/ctl.gleam` only parses `--control-file` after that subcommand. Valid examples use `scripts/scherzoctl ps --control-file <path> --json`.

- Observation: README already documented the control and observability CLI, including local mutating commands, but initially did not document pi as an operator UI.
  Evidence: Before this phase, `README.md` had a `Control and observability` section with `scripts/scherzoctl` examples and no `scherzo-operator` or `/skill:scherzo-operator` text. This phase added the `Using pi as an operator UI` section.

- Observation: The baseline test suite prints Erlang crash reports during successful runs.
  Evidence: `direnv exec . gleam test` prints crash reports from tests that intentionally panic worker/effect processes, then finishes with `533 passed, no failures`.

- Observation: The new documentation tests gave a useful staged signal during implementation.
  Evidence: Before the skill existed, `direnv exec . gleam test` ended with `533 passed, 5 failures`; after adding only the skill and reference, it ended with `537 passed, 1 failures`, leaving only the README operator-flow test red.

- Observation: Manual pi skill loading was not exercised in this workflow.
  Evidence: The optional dry-run would require an interactive live pi session; the implementation stopped at deterministic validation and recorded the skip reason instead.

## Decision Log

- Decision: Ship a project skill that uses command-first `scripts/scherzoctl ... --json` forms instead of a custom pi extension.
  Rationale: The control API and CLI already provide a stable programmatic interface. A skill is smaller, reviewable, and avoids another protocol layer.
  Date: 2026-04-28

- Decision: Place the skill under `.pi/skills/scherzo-operator/` without changing `.pi/settings.json`.
  Rationale: The repository already uses `.pi/skills/`, pi discovers directory skills there, and changing user-facing settings is unnecessary. Operators can use `/skill:scherzo-operator` when skill commands are enabled or start pi with `--skill .pi/skills/scherzo-operator` otherwise.
  Date: 2026-05-02

- Decision: Put detailed command examples in a reference file and keep `SKILL.md` focused on behavior and safety.
  Rationale: Pi skills use progressive disclosure. The main skill should fit cheaply in context, while the command list can be loaded only when needed.
  Date: 2026-04-28

- Decision: Require explicit confirmation before every state-changing command, including apparently benign commands such as `resume` and `reload`.
  Rationale: A pi operator session can affect a live daemon. Uniform confirmation is easier for a model to follow than a nuanced distinction between destructive and merely mutating commands.
  Date: 2026-05-02

- Decision: Prefer `events --json` over `attach --json` for summaries.
  Rationale: `attach` follows by default and can keep a tool call running. `events --json` returns a bounded page and is the safer default for agent summarization.
  Date: 2026-05-02

- Decision: Validate the skill with deterministic text tests rather than trying to automate model behavior.
  Rationale: Tests can reliably catch broken frontmatter, missing safety rules, and stale command references. Whether a model follows the skill remains a manual validation concern.
  Date: 2026-05-02

- Decision: Skip the optional manual pi dry-run during this non-interactive implementation workflow.
  Rationale: Launching a live pi session from the workflow would be interactive and could invoke a model unnecessarily. The README and skill now document both load paths, and deterministic tests cover those strings; a human operator can still perform the dry-run later.
  Date: 2026-05-02

- Decision: Do not create the plan's suggested commit from inside the Scherzo `workflow:execplan-implementation` workspace.
  Rationale: The workflow contract says not to create jj or git commits; Scherzo's publish step creates the final logical jj commit after review and validation.
  Date: 2026-05-02

## Outcomes & Retrospective

Implementation is complete in this working tree. The repository now has `.pi/skills/scherzo-operator/SKILL.md`, `.pi/skills/scherzo-operator/references/commands.md`, deterministic tests in `test/skill_docs_test.gleam`, and a README `Using pi as an operator UI` section. The tests validate skill frontmatter, safe operating rules, mutating-command confirmation language, command-first `scherzoctl --json` examples, response statuses, and both pi load paths. Final validation passed with `direnv exec . gleam format --check src test` and `direnv exec . gleam test`, which reported `538 passed, no failures` with the expected crash-report noise. Post-review validation repeated the format check and full test suite with the same `538 passed, no failures` result. The optional manual pi dry-run was skipped because this workflow is non-interactive; a future human operator can still load `/skill:scherzo-operator` or start `pi --skill .pi/skills/scherzo-operator` to observe model behavior against a running daemon. No typed pi extension is needed for this phase.

## Context and Orientation

Scherzo is a Gleam/Erlang daemon that polls Linear and runs pi coding-agent workflows in per-issue workspaces. Operators supervise a running daemon with `scripts/scherzoctl`, a POSIX shell wrapper that invokes `direnv exec <repo-root> gleam run -- ctl ...` from the repository root. The wrapper is the preferred command in documentation because it uses the repository's direnv/devenv toolchain consistently.

Pi skills are self-contained capability packages. A project skill can live under `.pi/skills/<skill-name>/SKILL.md`. The `SKILL.md` frontmatter must include `name` and `description`. The name must be lowercase letters, numbers, and hyphens, must not have leading, trailing, or consecutive hyphens, must be at most 64 characters, and must match the parent directory. The description must be present and at most 1024 characters. Pi scans skills at startup and exposes them as `/skill:<name>` commands when skill commands are enabled. A user can also explicitly load a skill directory with `pi --skill .pi/skills/scherzo-operator`.

The current Scherzo control CLI is implemented in `src/scherzo/ctl.gleam`, with the development wrapper in `scripts/scherzoctl`. Read-only inspection commands are `ping`, `ps`, `session <session-id>`, `events <session-id>`, and `attach <session-id>`. Non-streaming commands accept `--json` for protocol JSON. `events --json <session-id>` returns one bounded JSON response containing an event page. `attach --json <session-id>` prints one JSON stream event envelope per event and follows by default, so it is for live watching rather than normal summaries unless `--no-follow` is used.

The current mutating local commands are `pause`, `resume`, `reload`, `retry <issue>`, `park <issue> --reason <text> --yes`, `unpark <issue>`, `abort <session-id> --yes`, `stop-after-turn <session-id> --yes`, `prompt <session-id> <text>`, and `ui respond <session-id> <request-id> (--cancel | --value <text>)`. These commands all route through the authenticated local control API and return command result statuses defined in `src/scherzo/control/command.gleam`: `applied`, `queued`, `rejected`, `not_found`, and `not_allowed`.

The daemon writes a control file containing `host`, `port`, `token`, `workspace_root`, and `started_at_ms`. The default discovery path in `src/scherzo/control/file.gleam` is `.scherzo/workspaces/.scherzo-state/control.json`. `scripts/scherzoctl` can also discover the file from `SCHERZO_CONTROL_FILE`, or a command can pass `--control-file <path>` after the subcommand, such as `scripts/scherzoctl ps --control-file .scherzo/workspaces/.scherzo-state/control.json --json`. The skill must never reveal the token value from that file.

## Preconditions and Verified Facts

The repository uses direnv/devenv. If `direnv exec . <command>` reports that `.envrc` is blocked, read `.envrc`, run `direnv allow .` from the repository root, and retry the command. The current `.envrc` loads devenv and optional local `.env` files.

The current working tree now has `.pi/skills/scherzo-operator/SKILL.md`, `.pi/skills/scherzo-operator/references/commands.md`, and `test/skill_docs_test.gleam`. The new skill is a sibling of the existing project skills under `.pi/skills/`; the existing `exec-plan`, `exec-plan-review`, and Gleam review skills remain unchanged.

The current `scripts/scherzoctl` wrapper exists and delegates to `direnv exec "$ROOT" gleam run -- ctl "$@"`. `src/scherzo/ctl.gleam` currently supports all command names listed in this plan. `src/scherzo/control/command.gleam` currently defines the command result statuses `applied`, `queued`, `rejected`, `not_found`, and `not_allowed`. `src/scherzo/control/protocol.gleam` wraps successful command result payloads as `ok: true` responses and reserves `ok: false` for protocol, authentication, timeout, and malformed-request failures.

`README.md` has `Control and observability`, `Using pi as an operator UI`, `Linear command comments`, and safety sections. The operator UI section mentions `.pi/skills/scherzo-operator`, `/skill:scherzo-operator`, `pi --skill .pi/skills/scherzo-operator`, `SCHERZO_CONTROL_FILE`, and `scripts/scherzoctl ps --json`.

At plan-revision time, `direnv exec . gleam test` passed with `533 passed, no failures`. After adding the five `skill_docs` tests, final validation passed with `538 passed, no failures`. If unrelated work changes the count later, treat the pass/fail result and the presence of the `skill_docs` tests as authoritative.

## Scope Boundaries

In scope: the project skill at `.pi/skills/scherzo-operator/SKILL.md`; a command reference at `.pi/skills/scherzo-operator/references/commands.md`; deterministic Gleam tests in `test/skill_docs_test.gleam`; and a README section that tells operators how to load and use the skill.

Out of scope: custom pi extensions, typed pi tool definitions, `.pi/settings.json` changes, a web dashboard, remote control, broad filesystem discovery of control files, changing Scherzo's control protocol, changing scheduler semantics, changing `scripts/scherzoctl`, and automatically executing mutating commands without confirmation.

The existing `.pi/skills/exec-plan*` and `.pi/skills/gleam-*` directories stay unchanged. The existing `README.md` control command documentation stays as the canonical human CLI reference; the new README section should link the pi workflow to that control surface rather than replacing it.

## Milestones

Milestone 1 adds tests that describe the desired skill package before the package exists. At the end of the milestone, `test/skill_docs_test.gleam` fails because `.pi/skills/scherzo-operator/SKILL.md`, the command reference, and the README section do not exist yet. This proves the tests can catch the absence of the skill.

Milestone 2 adds the skill and command reference. At the end, `.pi/skills/scherzo-operator/SKILL.md` has valid frontmatter, concise safety-oriented instructions, and a relative link to `references/commands.md`. The reference file lists the current `scherzoctl` commands, command-first examples, JSON response guidance, and status interpretation. The skill tests for frontmatter, safety, and command references should pass.

Milestone 3 updates README and performs validation. At the end, README has a `Using pi as an operator UI` section, `direnv exec . gleam format --check src test` passes, `direnv exec . gleam test` passes, and a manual pi dry-run is either recorded or explicitly skipped with a reason.

## Plan of Work

Create `test/skill_docs_test.gleam` first. Use `simplifile.read` to read `.pi/skills/scherzo-operator/SKILL.md`, `.pi/skills/scherzo-operator/references/commands.md`, and `README.md`. The tests should assert literal strings that are important for discovery and safety rather than trying to parse Markdown completely.

Create `.pi/skills/scherzo-operator/SKILL.md`. The frontmatter name is `scherzo-operator`. Use a one-line description under 1024 characters. The body should tell the agent when to use the skill, the safe inspection sequence, how to locate the control file, which commands are read-only, which commands require confirmation, how to handle JSON responses, and what never to reveal. It should link to `references/commands.md` for detailed command examples.

Create `.pi/skills/scherzo-operator/references/commands.md`. Include command-first examples for `ping`, `ps`, `session`, `events`, `attach`, `pause`, `resume`, `reload`, `retry`, `park`, `unpark`, `abort`, `stop-after-turn`, `prompt`, and `ui respond`. Include `direnv exec . gleam run -- ctl ...` as a fallback if the wrapper is unavailable. Explain that non-streaming commands can use `--json`, that `attach --json` streams and follows by default, and that response statuses mean `applied`, `queued`, `rejected`, `not_found`, and `not_allowed`.

Update `README.md` with a `Using pi as an operator UI` section near `Control and observability`, after the basic `scherzoctl` commands are introduced. Explain how to start the daemon, find or export the control file path, start pi in another terminal from the repository root, load the skill with `/skill:scherzo-operator` or `pi --skill .pi/skills/scherzo-operator`, ask for a summary, and confirm before any mutation.

## Concrete Steps

1. From the repository root, inspect the tree state with `jj status --no-pager` if jj is available, or `git status --short` otherwise. Expect no unrelated changes before starting. If unrelated changes exist, do not overwrite them.

2. From the repository root, run `direnv exec . gleam test`. If direnv reports `.envrc is blocked`, run `direnv allow .` and retry. At plan-revision time the expected ending line is:

       533 passed, no failures

   Crash reports may appear during this successful run because some tests intentionally exercise crash handling.

3. Create `test/skill_docs_test.gleam`. Add `scherzo_operator_skill_frontmatter_is_valid_test`. It should read `.pi/skills/scherzo-operator/SKILL.md` and assert the file starts with `---\n`, contains `\nname: scherzo-operator\n`, contains `description:`, contains `# Scherzo Operator`, links to `references/commands.md`, and does not contain placeholders such as `TODO`, `TBD`, or `[CLARIFY]`.

4. In `test/skill_docs_test.gleam`, add `scherzo_operator_skill_requires_safe_operating_rules_test`. It should assert the skill text contains `scripts/scherzoctl`, `--json`, `SCHERZO_CONTROL_FILE`, `--control-file`, `events --json`, `ask`, `confirm`, `never reveal`, `token`, and each mutating command name: `pause`, `resume`, `reload`, `retry`, `park`, `unpark`, `abort`, `stop-after-turn`, `prompt`, and `ui respond`.

5. In `test/skill_docs_test.gleam`, add `scherzo_operator_reference_matches_current_ctl_surface_test`. It should read `.pi/skills/scherzo-operator/references/commands.md` and assert it contains command-first examples such as `scripts/scherzoctl ping --json`, `scripts/scherzoctl ps --json`, `scripts/scherzoctl session <session-id> --json`, `scripts/scherzoctl events <session-id> --json`, `scripts/scherzoctl attach --json --no-follow <session-id>`, `scripts/scherzoctl pause --json`, `scripts/scherzoctl resume --json`, `scripts/scherzoctl reload --json`, `scripts/scherzoctl retry ABC-123 --json`, `scripts/scherzoctl park ABC-123 --reason`, `scripts/scherzoctl unpark ABC-123 --json`, `scripts/scherzoctl abort <session-id> --yes --json`, `scripts/scherzoctl stop-after-turn <session-id> --yes --json`, `scripts/scherzoctl prompt <session-id>`, and `scripts/scherzoctl ui respond <session-id> <request-id>`. It should also assert the reference does not contain the invalid top-level option form `scripts/scherzoctl --control-file`.

6. In `test/skill_docs_test.gleam`, add `scherzo_operator_reference_explains_response_statuses_test`. It should assert the reference contains `applied`, `queued`, `rejected`, `not_found`, `not_allowed`, `ok: true`, and `ok: false`.

7. In `test/skill_docs_test.gleam`, add `readme_documents_pi_operator_skill_test`. It should read `README.md` and assert it contains `Using pi as an operator UI`, `/skill:scherzo-operator`, `pi --skill .pi/skills/scherzo-operator`, `SCHERZO_CONTROL_FILE`, and `scripts/scherzoctl ps --json`.

8. Run `direnv exec . gleam test`. Expect the new tests to fail because the skill and README section have not been added yet. The failure proves the red phase is meaningful.

9. Create `.pi/skills/scherzo-operator/` and `.pi/skills/scherzo-operator/references/`.

10. Write `.pi/skills/scherzo-operator/SKILL.md` with this frontmatter:

       ---
       name: scherzo-operator
       description: Operate a running Scherzo daemon from pi by using scherzoctl to inspect sessions, summarize worker progress, and perform confirmed operator controls. Use when the user asks to inspect Scherzo, summarize sessions, attach to workers, pause or resume dispatch, abort or stop workers, send follow-up prompts, retry, park or unpark issues, or answer Scherzo UI requests.
       ---

    Then add concise instructions under `# Scherzo Operator`. Include the safe inspection sequence, confirmation requirements, token secrecy, and a link to `references/commands.md`.

11. Write `.pi/skills/scherzo-operator/references/commands.md` with command examples. Use command-first forms such as `scripts/scherzoctl ps --json --control-file <path>` and never use `scripts/scherzoctl --control-file <path> ps --json`.

12. Run `direnv exec . gleam test`. Expect the skill and reference tests to pass and the README test to still fail if README has not been updated yet.

13. Update `README.md` by adding `## Using pi as an operator UI` near the existing `Control and observability` section. Include a short workflow: start Scherzo daemon, copy the control file path from the `control_server_started` log or export `SCHERZO_CONTROL_FILE`, start pi from the repository root, load `/skill:scherzo-operator` or start `pi --skill .pi/skills/scherzo-operator`, ask for a summary, and confirm before mutations.

14. Run `direnv exec . gleam format --check src test`. Expect formatting to pass. If the new test file needs formatting, run `direnv exec . gleam format src test` and then rerun the check.

15. Run `direnv exec . gleam test`. Expect all tests to pass. If no unrelated tests were added or removed, the count should be five higher than the baseline, approximately `538 passed, no failures`.

16. If pi is available, perform a manual skill-load dry-run from the repository root. Use either interactive `/skill:scherzo-operator` or explicit loading:

       pi --skill .pi/skills/scherzo-operator

    Ask: `Without executing commands, what Scherzo command would you run first to summarize current sessions?` The expected answer should mention `scripts/scherzoctl ps --json` and, if no control file is known, should ask for `SCHERZO_CONTROL_FILE` or a `--control-file` path. If pi is not available or interactive validation is inappropriate, record the skip reason in Outcomes & Retrospective.

17. Update the Progress section of this plan with the final test output and manual validation result. Update Surprises & Discoveries or Decision Log if implementation reveals drift.

18. In this Scherzo `workflow:execplan-implementation` workspace, do not create a jj or git commit; the publish step creates the final logical jj commit after review and validation. If a human implements this plan outside that workflow, one commit with a message such as `Add Scherzo operator pi skill` is sufficient because the change is additive documentation plus its validation tests; do not commit the intentionally failing red phase separately.

## Testing and Falsifiability

The skill is falsified if pi cannot discover it because frontmatter is invalid, if it omits `scherzoctl`, if it tells the agent to parse pretty output instead of JSON, if it recommends blocking `attach` for ordinary summaries, if it omits confirmation for state-changing commands, if it encourages revealing control tokens, if examples use invalid option ordering, or if the reference commands do not match the implemented `scherzoctl` interface.

Add deterministic tests in `test/skill_docs_test.gleam` as described in the concrete steps. These tests should read the actual Markdown files and assert the strings that make the package safe and discoverable. Use exact command examples to keep the reference synchronized with `src/scherzo/ctl.gleam`.

Run from the repository root:

    direnv exec . gleam format --check src test
    direnv exec . gleam test

Manual validation with a real pi session is recommended but optional. It should verify skill loading and initial command choice, not live daemon mutation. If skipped, record that in Outcomes & Retrospective.

## Validation and Acceptance

Accept this phase when `.pi/skills/scherzo-operator/SKILL.md` has valid skill frontmatter, links to `references/commands.md`, and includes read-only-first, JSON-first, confirmation, and secrecy rules.

Accept this phase when `.pi/skills/scherzo-operator/references/commands.md` lists the current `scherzoctl` commands with command-first examples, explains that `attach --json` streams and follows by default, and explains both protocol response failures and command result statuses.

Accept this phase when `README.md` explains how to use a separate pi session as the operator UI, including both `/skill:scherzo-operator` and `pi --skill .pi/skills/scherzo-operator`.

Accept this phase when `direnv exec . gleam format --check src test` and `direnv exec . gleam test` pass. At plan-revision time the baseline is 533 tests; with the five new tests in this plan and no unrelated changes, expect about 538 passing tests.

A representative operator workflow should be documented as:

    export SCHERZO_CONTROL_FILE=.scherzo/workspaces/.scherzo-state/control.json
    pi
    /skill:scherzo-operator
    summarize current Scherzo sessions

The skill should choose `scripts/scherzoctl ps --json` before suggesting any mutating command.

## Rollout, Recovery, and Idempotence

The skill is additive. It does not change daemon startup, the control API, scheduler state, Linear integration, or pi RPC behavior. If it behaves poorly, operators can ignore it and continue using `scripts/scherzoctl` directly. Removing `.pi/skills/scherzo-operator/` disables the project skill. Reverting the README section and `test/skill_docs_test.gleam` restores the previous tree.

The skill should not store state. Each pi session reads the current control file and calls `scherzoctl` as needed. Re-running read-only commands is safe. Mutating commands are not idempotent in general, so the skill must confirm intent, use the current full session or issue id, and report the command response status back to the user.

If slash skill commands are disabled, no rollback is required. Operators can either enable skill commands in pi settings or start pi with `pi --skill .pi/skills/scherzo-operator`.

## Artifacts and Notes

Current `scherzoctl` help confirms the command surface to document:

    Usage: gleam run -- ctl <command> [options]
    Local Scherzo daemon inspection and operator controls. Commands:
      ping
      ps
      session <session-id>
      events <session-id>
      attach <session-id>
      pause
      resume
      reload
      retry <issue>
      park <issue> --reason <text> --yes
      unpark <issue>
      abort <session-id> --yes
      stop-after-turn <session-id> --yes
      prompt <session-id> <text>
      ui respond <session-id> <request-id> (--cancel | --value <text>)

Current baseline validation succeeded with expected crash-report noise:

    direnv exec . gleam test
    ...
    533 passed, no failures

The implementation red phase and staged validation behaved as expected:

    direnv exec . gleam test
    ...
    533 passed, 5 failures

    direnv exec . gleam test
    ...
    537 passed, 1 failures

Final validation succeeded:

    direnv exec . gleam format --check src test

    direnv exec . gleam test
    ...
    538 passed, no failures

Post-review validation also succeeded after checking the Gleam review feedback:

    direnv exec . gleam format --check src test

    direnv exec . gleam test
    ...
    538 passed, no failures

Example skill workflow:

    User: /skill:scherzo-operator
    User: summarize current workers
    Agent: runs scripts/scherzoctl ps --json
    Agent: runs scripts/scherzoctl events <session-id> --json for active sessions when detail is needed
    Agent: summarizes progress and asks before any intervention

Future extension point: if CLI-based operation is too clumsy, a later plan can add a custom pi extension with typed tools equivalent to `scherzo_list_sessions`, `scherzo_get_events`, `scherzo_send_prompt`, and `scherzo_abort_session`. That is deliberately deferred here.

## Interfaces and Dependencies

The new skill file path is:

    .pi/skills/scherzo-operator/SKILL.md

The new reference file path is:

    .pi/skills/scherzo-operator/references/commands.md

The new test file path is:

    test/skill_docs_test.gleam

The skill depends on the existing `scherzoctl` interface and should prefer these command shapes:

    scripts/scherzoctl ping --json
    scripts/scherzoctl ps --json
    scripts/scherzoctl session <session-id> --json
    scripts/scherzoctl events <session-id> --json
    scripts/scherzoctl events <session-id> --json --since-cursor <n>
    scripts/scherzoctl attach --json --no-follow <session-id>
    scripts/scherzoctl pause --json
    scripts/scherzoctl resume --json
    scripts/scherzoctl reload --json
    scripts/scherzoctl retry ABC-123 --json
    scripts/scherzoctl park ABC-123 --reason "manual cleanup" --yes --json
    scripts/scherzoctl unpark ABC-123 --json
    scripts/scherzoctl abort <session-id> --yes --json
    scripts/scherzoctl stop-after-turn <session-id> --yes --json
    scripts/scherzoctl prompt <session-id> "message" --json
    scripts/scherzoctl ui respond <session-id> <request-id> --cancel --json
    scripts/scherzoctl ui respond <session-id> <request-id> --value "ok" --json

When an explicit control file is needed, the option belongs after the subcommand:

    scripts/scherzoctl ps --json --control-file .scherzo/workspaces/.scherzo-state/control.json

If the wrapper is unavailable, the fallback is:

    direnv exec . gleam run -- ctl ps --json

No new runtime package dependency is required. The tests use the existing `simplifile` dependency already present in `gleam.toml`.
